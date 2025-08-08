{-# LANGUAGE OverloadedStrings #-}

module Fmm.Mods where

import Fmm.Types

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wreq

getMod :: String -> IO (Maybe Mod)
getMod name = do
  r <-
    getWith
      (set checkResponse (Just $ \_ _ -> return ()) defaults)
      ("http://mods.factorio.com/api/mods/" ++ name)
  if r ^. responseStatus . statusCode /= 200
    then pure Nothing
    else do
      j :: Response Mod <- asJSON r
      pure $ Just $ j ^. responseBody

getMods :: Maybe Text -> Int -> IO (Pagination, [Mod])
getMods fVersion page = do
  r <-
    getWith
      ( defaults
          & param "version" .~ maybeToList fVersion
          & param "page" .~ [T.pack $ show page]
      )
      "http://mods.factorio.com/api/mods"

  ml :: Value <- asJSON r <&> (^. responseBody)

  let pagination = fromSuccess $ fromJSON $ ml ^?! key "pagination"

  let mods :: [Mod] = fromSuccess $ fromJSON $ ml ^?! key "results"

  pure (pagination, mods)
 where
  fromSuccess (Success a) = a
  fromSuccess (Error s) = error s
