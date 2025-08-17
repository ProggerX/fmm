{-# LANGUAGE OverloadedStrings #-}

module Fmm.Instances where

import Fmm.Types

import Control.Lens hiding (uncons)
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as BS
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmm.Downloader
import Fmm.Mods
import System.Directory
import System.IO (readFile')
import System.Process

createInstance :: Text -> Text -> Text -> IO ()
createInstance name version edition = do
  vs <- getVersions

  when (version `elem` vs) $ do
    dp <- downloadVersion version edition
    home <- getHomeDirectory
    case dp of
      (Just p) -> do
        let cp = home ++ "/.fmm/instances/" ++ T.unpack name
        createDirectory (cp ++ "tmp")
        callProcess "/usr/bin/env" ["-S", "tar", "-xf", p, "-C", cp ++ "tmp"]
        callProcess "/usr/bin/env" ["-S", "mv", cp ++ "tmp/factorio", cp]
        callProcess "/usr/bin/env" ["-S", "rm", "-r", cp ++ "tmp"]
      Nothing -> pure ()

renameInstance :: Instance -> Text -> IO ()
renameInstance = TIO.writeFile . (++ "/.fmm-name") . ipath

hd :: [a] -> a
hd = fst . fromJust . uncons

syncMods :: Instance -> [Mod] -> IO ()
syncMods inst mods = do
  let mp = modsPath inst

  let releases = map latest_release mods
  let files = map file_name releases

  mlExists <- doesFileExist $ mp ++ "mod-list.json"
  when mlExists $ removeFile (mp ++ "mod-list.json")

  removeUnused mp files
  downloadNeeded (fVersion inst) mp mods []
 where
  removeUnused ms fs =
    listDirectory ms
      >>= mapM_
        ( \p -> do
            let found = find (== p) fs
            case found of
              Nothing -> removeFile $ ms ++ p
              _ -> pure ()
        )
        . filter ((== 'p') . last)
  downloadNeeded :: Text -> FilePath -> [Mod] -> [String] -> IO ()
  downloadNeeded fv mp rels used = forM_ rels $ \m -> do
    let r = latest_release m
    fp <- downloadRelease r
    e <- doesFileExist (mp ++ file_name r)
    when (not e && isJust fp) $ createFileLink (fromJust fp) (mp ++ file_name r)

    let depends =
          dependencies r
            & map T.unpack
            & filter (flip notElem ("?!" :: String) . hd)
            & filter (flip notElem ("?!" :: String) . hd . drop 1)
            & map (hd . words . (\x -> if hd x == '~' then drop 2 x else x))
            & filter (/= "base")
            & filter (`notElem` used)

    mmods <- mapM (getMod fv) depends
    downloadNeeded fv mp (map fromJust mmods) (T.unpack (name m) : used)

getVersion :: FilePath -> IO Text
getVersion path = do
  vf <- BS.readFile $ path ++ "/data/base/info.json"
  let v :: Value = fromJust $ decode vf

  pure $ v ^?! key "version" . _String

getInstances :: IO [Instance]
getInstances = do
  home <- getHomeDirectory
  st <- maybeToList <$> getSteamInstance home

  let instPath = home ++ "/.fmm/instances/"
  createDirectoryIfMissing True instPath
  files <- map (instPath ++) <$> listDirectory instPath

  insts <- mapM makeInstance files
  pure $ st ++ insts
 where
  makeInstance :: FilePath -> IO Instance
  makeInstance p = do
    v <- getVersion p
    n <- doesFileExist (p ++ "/.fmm-name")
    unless n $ writeFile (p ++ "/.fmm-name") (reverse $ takeWhile (/= '/') $ reverse p)

    name <- takeWhile (/= '\n') <$> readFile' (p ++ "/.fmm-name")

    pure $
      Instance
        { iname = T.pack name
        , ipath = p
        , modsPath = p ++ "/mods/"
        , binPath = p ++ "/bin/x64/factorio"
        , fVersion = v
        }

getSteamInstance :: FilePath -> IO (Maybe Instance)
getSteamInstance home = do
  let p = home ++ "/.factorio"
  let bp = home ++ "/.local/share/Steam/steamapps/common/Factorio/bin/x64/factorio"
  t <- doesFileExist bp
  if t
    then do
      v <- getVersion (home ++ "/.local/share/Steam/steamapps/common/Factorio")
      n <- doesFileExist (p ++ "/.fmm-name")
      unless n $ writeFile (p ++ "/.fmm-name") "Steam"
      name <- takeWhile (/= '\n') <$> readFile' (p ++ "/.fmm-name")

      pure . Just $
        Instance
          { iname = T.pack name
          , ipath = home ++ "/.factorio/"
          , modsPath = home ++ "/.factorio/mods/"
          , binPath = bp
          , fVersion = v
          }
    else pure Nothing
