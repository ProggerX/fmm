{-# LANGUAGE OverloadedStrings #-}

module Fmm.Mods where

import Fmm.Types

import Control.Lens
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
