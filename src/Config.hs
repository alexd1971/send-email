{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Control.Monad.Reader
import           Data.Aeson                     ( FromJSON )
import           GHC.Generics                   ( Generic )

data Config = Config {  server      :: String
                      , tlsPort     :: Integer
                      , login       :: String
                      , password    ::String
                     } deriving Generic

instance FromJSON Config

type ConfigReader = ReaderT Config IO
