module Fmm.Launcher where

import Fmm.Types

import Control.Monad
import System.Environment
import System.Process

isNixos :: IO Bool -- NOTE: Very unstable
isNixos = (/= "") <$> getEnv "NIX_PROFILES"

launchGame :: Instance -> IO ()
launchGame i = do
  steamRun <- isNixos

  let p =
        if steamRun
          then proc "/usr/bin/env" ["-S", "steam-run", binPath i]
          else proc (binPath i) []

  void $ createProcess p
