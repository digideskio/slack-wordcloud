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
import Data.Attoparsec.Text as Attoparsec
import Data.Aeson
import Data.Aeson.Lens
import Data.Char
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
      runSecureClient host 443 path' (client pool)

client :: ConnectionPool -> WebSockets.Connection -> IO ()
client pool conn = forever $ do
  input <- receiveData conn
  case decode input of
    Just (msg :: Value) -> (flip runSqlPool pool) $ do
      when (Just "message" == msg ^? key "type" . _String) $ do
        let body = msg ^. key "text" . _String
            channel = msg ^. key "channel" . _String
        liftIO $ print msg
        if "!wordrank" `T.isPrefixOf` body
          then do
            wordRank <- getWordRank channel
            liftIO $ print wordRank
            liftIO $ sendTextData conn (encode wordRank)
          else
            void $ traverse (trackWord channel) (extractWords body)
    Nothing -> return ()

trackWord :: MonadIO m => Text -> Text -> SqlPersistT m ()
trackWord channel word = do
  res <- selectFirst [SlackWordBody ==. word, SlackWordChannelName ==. channel] []
  case res of
    Just (Entity wordKey word) -> void $ update wordKey [SlackWordOccurences +=. 1]
    Nothing -> void $ insert $ SlackWord channel word 1

getWordRank :: MonadIO m => Text -> SqlPersistT m Value
getWordRank channel = do
  ranked <- selectList [SlackWordChannelName ==. channel] [Desc SlackWordOccurences, LimitTo 10]
  liftIO $ print (slackWordBody . entityVal <$> ranked)
  let ranked' = Prelude.zip (slackWordBody . entityVal <$> ranked) [1..]
      text = T.unlines $ (\(word, ix) -> T.pack (show ix) <> ". " <> word) <$> ranked'
  id <- liftIO (randomIO :: IO Word64)
  return $ object ["id" .= id, "channel" .= channel, "type" .= ("message" :: Text), "text" .= text]

words :: Parser [Text]
words = Attoparsec.takeWhile isAlphaNum `sepBy` anyChar

extractWords :: Text -> [Text]
extractWords input = Prelude.filter (not . T.null) $ parseOnly Main.words input ^. _Right

