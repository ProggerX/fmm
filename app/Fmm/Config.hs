{-# LANGUAGE OverloadedStrings #-}

module Fmm.Config where

-- import Control.Lens
-- import Network.Wreq
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory

-- tokenFromLoginPassword :: Text -> Text -> IO (Maybe String)
-- tokenFromLoginPassword login password = do
--   r <- getWith opts "https://auth.factorio.com/api-login"
--   pure Nothing
--  where
--   opts = defaults & param "login" .~ [login]

cSave :: String -> Text -> IO ()
cSave p txt = do
  home <- getHomeDirectory

  let fmmPath = home ++ "/.fmm/"
  createDirectoryIfMissing True fmmPath

  TIO.writeFile (fmmPath ++ p) txt

cGet :: String -> IO (Maybe Text)
cGet p = do
  home <- getHomeDirectory

  let fmmPath = home ++ "/.fmm/"
  createDirectoryIfMissing True fmmPath

  d <- doesFileExist (fmmPath ++ p)
  if d
    then Just <$> TIO.readFile (fmmPath ++ p)
    else pure Nothing

getDownload :: IO (Maybe (Text, Text))
getDownload = do
  x <- liftA2 (,) (cGet "login") (cGet "token")
  pure $ uncurry (liftA2 (,)) x
