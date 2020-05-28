{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp       ( run )

import           RequestHandler                 ( handler )

main :: IO ()
main = do
    let port = 7777
    putStrLn $ "Listening on port " ++ show port
    run port handler
