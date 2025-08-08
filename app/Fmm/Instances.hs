module Fmm.Instances where

import Fmm.Types

import Control.Monad
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Fmm.Downloader
import Fmm.Mods
import System.Directory

getInstances :: IO [Instance]
getInstances = do
  home <- getHomeDirectory
  st <- maybeToList <$> getSteamInstance home

  let instPath = home ++ "/.fmm/instances/"
  createDirectoryIfMissing True instPath
  files <- map (instPath ++) <$> listDirectory instPath

  pure $ st ++ map makeInstance files
 where
  makeInstance :: FilePath -> Instance
  makeInstance p =
    Instance
      { modsPath = p ++ "/mods/"
      , binPath = p ++ "/bin/x64/factorio/"
      , fVersion = T.pack "2.0"
      }

getSteamInstance :: FilePath -> IO (Maybe Instance)
getSteamInstance home = do
  let bp = home ++ "/.local/share/Steam/steamapps/common/Factorio/bin/x64/factorio"
  t <- doesFileExist bp
  if t
    then
      pure . Just $
        Instance
          { modsPath = home ++ "/.factorio/mods/"
          , binPath = bp
          , fVersion = T.pack "2.0"
          }
    else pure Nothing

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
