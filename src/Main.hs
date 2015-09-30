{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Config
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.Lens
import Data.Char
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Word
import Network.URI
import Network.WebSockets as WebSockets
import Network.Wreq
import System.Random
import Wuss

slackRtmStartUrl :: String
slackRtmStartUrl = "https://slack.com/api/rtm.start"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SlackWord sql=words
  channelName Text sql=channel
  body Text
  occurences Int
BannedWord sql=banned_words
  body Text
|]

createIndex :: MonadIO m => Text -> Text -> SqlPersistT m ()
createIndex name on = do
  res <- rawSql ("select to_regclass('" <> name <> "')::text") []
  case (unSingle $ Prelude.head res :: Maybe Text) of
    Just _ -> return ()
    Nothing -> rawExecute ("create index " <> name <> " on " <> on) []

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool databaseConnection 10 $ \pool -> do
  (flip runSqlPool pool) $ do
    runMigration migrateAll
    createIndex "index_words_on_channel" "words (channel)"
    createIndex "index_words_on_body" "words (body)"
    createIndex "index_words_on_occurences" "words (occurences)"
    createIndex "index_banned_words_on_body" "banned_words (body)"

  liftIO $ do
    response <- post slackRtmStartUrl
      [ "token" := slackToken,
        "no_unreads" := (1 :: Int),
        "simple_latest" := (1 :: Int)]

    when (Just True == response ^? responseBody . key "ok" . _Bool) $ do
      let inputUri = response ^. responseBody . key "url" . _String
          Just uri = parseURI $ T.unpack inputUri
          Just auth' = uriAuthority uri
          host = uriRegName auth'
          path' = uriPath uri
          commands = [wordrank, banWords, trackWords]
      runSecureClient host 443 path' (client pool commands)

isBanned :: MonadIO m => Text -> SqlPersistT m Bool
isBanned word = isJust <$> selectFirst [BannedWordBody ==. word] []

data Command = Command { commandMatches  :: Text -> Bool
                       , commandAction   :: Text -> Text -> SqlPersistT IO (Maybe Value)
                       -- , commandFollowup :: Maybe (Value -> SqlPersistT IO (Maybe Value))
                       }

wordrank :: Command
wordrank = Command matcher action
  where matcher = T.isPrefixOf "!wordrank"
        action channel _ = do
          ranked <- selectList [SlackWordChannelName ==. channel] [Desc SlackWordOccurences, LimitTo 10]
          liftIO $ print (slackWordBody . entityVal <$> ranked)
          let ranked' = Prelude.zip (entityVal <$> ranked) [(1 :: Word32)..]
              text = T.unlines $ (\(word, ix) -> T.pack (show ix) <> ". " <> slackWordBody word <> " - " <> T.pack (show (slackWordOccurences word))) <$> ranked'
          id <- liftIO (randomIO :: IO Word64)
          return $ pure $ object [ "id" .= id, "channel" .= channel
                                 , "type" .= ("message" :: Text), "text" .= text
                                 ]

trackWords :: Command
trackWords = Command (const True) action
  where action channel body = pure Nothing <* traverse (trackWord channel) (extractWords body)
        trackWord channel word = do
          res <- selectFirst [SlackWordBody ==. word, SlackWordChannelName ==. channel] []
          banned <- isBanned word
          unless banned $ do
            case res of
              Just (Entity wordKey _) -> update wordKey [SlackWordOccurences +=. 1]
              Nothing -> void $ insert $ SlackWord channel word 1

banWords :: Command
banWords = Command matcher action
  where matcher = T.isPrefixOf "!wordban"
        action _ body = do
          let words = extractWords (T.drop (T.length "!wordban") body)
          absent <- traverse isBanned words
          let toAdd = fst <$> Prelude.filter (not . snd) (Prelude.zip words absent)
              ban word = deleteWhere [SlackWordBody ==. word]
          void $ traverse (insert . BannedWord) toAdd
          void $ traverse ban toAdd
          pure Nothing

client :: ConnectionPool -> [Command] -> WebSockets.Connection -> IO ()
client pool commands conn = forever $ do
  input <- receiveData conn
  case decode input of
    Just (msg :: Value) -> (flip runSqlPool pool) $ do
      when (Just "message" == msg ^? key "type" . _String) $ do
        let body = msg ^. key "text" . _String
            channel = msg ^. key "channel" . _String
            action = commandAction <$> List.find (\c -> (commandMatches c) body) commands
        case action of
          Just act -> do
            msg <- act channel body
            maybe (pure ()) (liftIO . sendTextData conn . encode) msg
          Nothing -> pure ()
    Nothing -> pure ()

extractWords :: Text -> [Text]
extractWords input = filterEmpty $ fixPunctuation <$> filterUris parsed
  where filterUris xs = Prelude.filter (not . isURI . T.unpack) xs
        fixPunctuation = dropAround (\w -> isPunctuation w || isDigit w || isSymbol w)
        filterEmpty = Prelude.filter (not . T.null)
        parsed = T.split isSpace input

