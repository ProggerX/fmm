{-# LANGUAGE LambdaCase #-}

module Fmm where

import Control.Monad (unless, void)
import Fmm.Types
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.IO (hGetContents)
import System.Process
import Prelude hiding (mod)

newApp :: App
newApp = app &= program "fmm" &= summary "fmm v0.0.1, (c) ProggerX 2025"
 where
  app =
    modes
      [ usage &= auto
      , launcher &= name "l" &= name "launch"
      , modpacks &= name "mp" &= name "modpack"
      , mods &= name "m" &= name "mod"
      ]

launcher :: App
launcher = Launcher{edition = "steam" &= argPos 0 &= typ "VERSION" &= opt "steam"}

modpacks :: App
modpacks = ModPacks{}

mods :: App
mods = Mods{}

usage :: App
usage = Usage

runApp :: App -> IO ()
runApp = \case
  Launcher{edition} -> launchGame edition
  Usage -> printHelp
  _ -> undefined

isNixos :: IO Bool -- NOTE: Very unstable
isNixos = (/= "") <$> getEnv "NIX_PROFILES"

launchGame :: String -> IO ()
launchGame ed = do
  home <- getHomeDirectory

  let path =
        if ed == "steam"
          then home ++ "/.local/share/Steam/steamapps/common/Factorio/bin/x64/factorio"
          else home ++ "/.fmm/versions/" ++ ed ++ "/bin/x64/factorio"

  exists <- doesFileExist path
  unless exists $ do
    putStrLn $ "Path not found: " ++ path

  doSteamRun <- isNixos
  let pr =
        if doSteamRun
          then (proc "/usr/bin/env" ["-S", "steam-run", path]){std_out = CreatePipe}
          else (proc path []){std_out = CreatePipe}

  (_, Just hout, _, _) <- createProcess pr

  hGetContents hout >>= putStr

printHelp :: IO ()
printHelp = do
  exe <- getExecutablePath
  void $ createProcess $ proc exe ["--help"]
