{-# LANGUAGE OverloadedStrings #-}

module Fmm.Mods where

import Fmm.Types

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.List
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wreq
import System.Directory

getMod :: Text -> String -> IO (Maybe Mod)
getMod fVersion name = do
  home <- getHomeDirectory
  createDirectoryIfMissing True $ home ++ "/.fmm/cache"
  let cachepath = home ++ "/.fmm/cache/" ++ T.unpack fVersion ++ "_" ++ name
  cached <- doesFileExist cachepath

  if cached
    then readFile cachepath <&> Just . read
    else do
      r <-
        getWith
          (set checkResponse (Just $ \_ _ -> return ()) defaults)
          ("http://mods.factorio.com/api/mods/" ++ name ++ "/full")
      if r ^. responseStatus . statusCode /= 200
        then pure Nothing
        else do
          jr :: Response Value <- asJSON r
          let j = jr ^. responseBody

          let rels :: [Release] = fromSuccess $ fromJSON $ j ^?! key "releases"

          let rel' = find ((== fVersion) . factorioVersion) $ reverse rels

          case rel' of
            Just rel ->
              do
                let m =
                      Mod
                        { latest_release = rel
                        , name = j ^?! key "name" . _String
                        , title = j ^?! key "title" . _String
                        , summary = j ^?! key "summary" . _String
                        }
                writeFile cachepath (show m)
                pure $ Just m
            Nothing -> pure Nothing

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
