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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  parsedConfig <-
    decodeFileEither "config.yaml" :: IO (Either ParseException Config)
  case parsedConfig of
    Left  exception -> print exception
    Right config    -> runReaderT startServer config

startServer :: ConfigReader ()
startServer = do
  let port = 7777
  application <- mkApplication
  lift $ putStrLn $ "Listening on port " ++ show port
  lift $ run port $ application
