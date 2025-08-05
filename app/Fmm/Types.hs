module Fmm.Types where

import Data.Data

data App
  = Launcher {edition :: String}
  | ModPacks
  | Mods
  | Usage
  deriving (Data, Typeable)
