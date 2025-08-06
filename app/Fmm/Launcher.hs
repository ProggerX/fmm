{-# LANGUAGE OverloadedStrings #-}

module Fmm.Launcher where

import Control.Monad (unless, void)
import Data.Text (Text, pack)
import System.Directory
import System.Environment
import System.Process

isNixos :: IO Bool -- NOTE: Very unstable
isNixos = (/= "") <$> getEnv "NIX_PROFILES"

launchGame :: String -> IO ()
launchGame ed = do
  home <- getHomeDirectory

  let path =
        if ed == "steam"
          then home ++ "/.local/share/Steam/steamapps/common/Factorio/bin/x64/factorio"
          else home ++ "/.fmm/versions/" ++ ed ++ "/bin/x64/factorio"

  let modsDir = home ++ "/.factorio/mods"

  exists <- doesFileExist path
  unless exists $ do
    putStrLn $ "Path not found: " ++ path

  doSteamRun <- isNixos
  let pr =
        if doSteamRun
          then proc "/usr/bin/env" ["-S", "steam-run", path, "--mod-directory", modsDir]
          else proc path ["--mod-directory", modsDir]

  void $ createProcess pr

getVersions :: IO [Text]
getVersions = do
  home <- getHomeDirectory
  dir <- listDirectory $ home ++ "/.fmm/versions"
  steamExists <- doesFileExist $ home ++ "/.local/share/Steam/steamapps/common/Factorio/bin/x64/factorio"
  pure $ ["steam" | steamExists] ++ map pack dir
