{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Yaml                      ( ParseException
                                                , decodeFileEither
                                                )
import           Network.Wai.Handler.Warp       ( run )

import           Control.Monad.Reader
import           RequestHandler                 ( mkApplication )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )

import           Config
import           Logs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  setupLogger
  logInfo "Reading config..."
  parsedConfig <-
    decodeFileEither "config.yaml" :: IO (Either ParseException Config)
  case parsedConfig of
    Left  exception -> logError $ "Config error: " <> show exception
    Right config    -> do
      logInfo "...success"
      logInfo "Starting service..."
      runReaderT startServer config

startServer :: ConfigReader ()
startServer = do
  let port = 7777
  application <- mkApplication
  lift . logInfo $ "...started on port " <> show port
  lift . run port $ application
