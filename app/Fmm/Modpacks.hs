module Fmm.Modpacks where

import Codec.Archive.Zip
import Data.ByteString.Lazy qualified as BS
import Data.Text hiding (map)
import System.Directory

getModpacks :: IO [Text]
getModpacks = do
  home <- getHomeDirectory

  let modpacksPath = home ++ "/.fmm/modpacks"
  map pack <$> listDirectory modpacksPath

createModpack :: Text -> IO ()
createModpack name = do
  home <- getHomeDirectory

  let modsPath = home ++ "/.factorio/mods/"
  mods <- map (modsPath ++) <$> listDirectory modsPath

  archive <- addFilesToArchive [OptLocation "." False] emptyArchive mods

  let modpackPath = home ++ "/.fmm/modpacks/" ++ unpack name ++ ".zip"

  BS.writeFile modpackPath $ fromArchive archive

engageModpack :: Text -> IO ()
engageModpack name = do
  home <- getHomeDirectory

  let modpackPath = home ++ "/.fmm/modpacks/" ++ unpack name
  let modsPath = home ++ "/.factorio/mods/"

  bytes <- BS.readFile modpackPath
  let archive = toArchive bytes
  extractFilesFromArchive [OptDestination modsPath] archive
