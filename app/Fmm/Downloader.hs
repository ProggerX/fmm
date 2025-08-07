{-# LANGUAGE OverloadedStrings #-}

module Fmm.Downloader where

import Fmm.Config
import Fmm.Types

import Control.Lens
import Data.ByteString.Lazy qualified as BS
import Network.Wreq
import System.Directory

downloadRelease :: Release -> IO (Maybe FilePath)
downloadRelease rel = do
  let url = "http://mods.factorio.com" ++ download_url rel

  d <- getDownload

  case d of
    Nothing -> do
      putStrLn "can't read config"
      pure Nothing
    Just (login, token) -> do
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
          home <- getHomeDirectory
          createDirectoryIfMissing True $ home ++ "/.fmm/mods"

          let fp = home ++ "/.fmm/mods/" ++ file_name rel
          BS.writeFile fp (r ^. responseBody)
          pure $ Just fp
