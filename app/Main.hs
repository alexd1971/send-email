{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp       ( run )

import           RequestHandler                 ( handler )
import           System.IO                      ( hSetBuffering
                                                , stdout
                                                , BufferMode(NoBuffering)
                                                )

main :: IO ()
main = do
    let port = 7777
    hSetBuffering stdout NoBuffering
    putStrLn $ "Listening on port " ++ show port
    run port handler
