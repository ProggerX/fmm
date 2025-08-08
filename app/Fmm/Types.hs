{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fmm.Types where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text)
import GHC.Generics (Generic)

data Instance = Instance
  { iname :: Text
  , modsPath :: FilePath
  , binPath :: FilePath
  , fVersion :: Text
  }
  deriving (Show)

data Mod = Mod
  { name :: Text
  , title :: Text
  , summary :: Text
  , latest_release :: Release
  }
  deriving (FromJSON, Show, Generic, Read)

data Pagination = Pagination
  { count :: Int
  , page_count :: Int
  }
  deriving (FromJSON, Show, Generic)

data Release = Release
  { info_json :: Object
  , version :: String
  , download_url :: String
  , file_name :: String
  }
  deriving (FromJSON, Show, Generic, Read)

factorioVersion :: Release -> Text
factorioVersion = (^?! key "factorio_version" . _String) . Object . info_json

fromSuccess :: Result a -> a
fromSuccess (Success a) = a
fromSuccess (Error s) = error s

dependencies :: Release -> [Text]
dependencies = fromSuccess . fromJSON . (^?! key "dependencies") . Object . info_json
