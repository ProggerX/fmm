{-# LANGUAGE OverloadedStrings #-}

module Fmm.Downloader where

import Fmm.Config
import Fmm.Types

import Control.Lens
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Network.Wreq
import System.Directory
import Text.Regex.TDFA

getVersions :: IO [Text]
getVersions = do
  r <- get "http://factorio.com/download/archive"
  let html = decodeUtf8 $ toStrict $ r ^. responseBody

  let regex = "href=\"/download/archive/.*\"" :: String
      matches =
        map (T.pack . drop 3 . dropWhile (/= 'v') . init) $
          getAllTextMatches $
            T.unpack html =~ regex

  pure matches

downloadVersion :: Text -> Text -> IO (Maybe FilePath)
downloadVersion v ed = do
  let url = T.unpack $ T.concat ["http://factorio.com/get-download/", v, "/", ed, "/linux64"]
  putStrLn $ "Downloading from " ++ url

  home <- getHomeDirectory
  createDirectoryIfMissing True $ home ++ "/.fmm/versions"

  let fp = T.unpack $ T.concat [T.pack home, "/.fmm/versions/", v, "_", ed, ".tar.xz"]

  d <- getDownload
  exists <- doesFileExist fp

  case (d, exists) of
    (Just (login, token), False) -> do
      r <-
        getWith
          ( defaults
              & param "username"
                .~ [login]
              & param "token"
                .~ [token]
          )
          url

      if r ^. responseStatus . statusCode /= 200
        then do
          putStrLn "can't download"
          pure Nothing
        else do
          BS.writeFile fp (r ^. responseBody)
          pure $ Just fp
    (_, True) -> do
      putStrLn $ "Already exists: " ++ fp
      pure $ Just fp
    _ -> pure Nothing

downloadRelease :: Release -> IO (Maybe FilePath)
downloadRelease rel = do
  let url = "http://mods.factorio.com" ++ download_url rel

  home <- getHomeDirectory
  createDirectoryIfMissing True $ home ++ "/.fmm/mods"

  let fp = home ++ "/.fmm/mods/" ++ file_name rel

  d <- getDownload
  exists <- doesFileExist fp

  case (d, exists) of
    (Just (login, token), False) -> do
      r <-
        getWith
          ( defaults
              & param "username"
                .~ [login]
              & param "token"
                .~ [token]
          )
          url

      if r ^. responseStatus . statusCode /= 200
        then do
          putStrLn "can't download"
          pure Nothing
        else do
          BS.writeFile fp (r ^. responseBody)
          pure $ Just fp
    (_, True) -> pure $ Just fp
    _ -> pure Nothing
