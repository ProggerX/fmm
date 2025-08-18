{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI where

import Fmm.Downloader
import Fmm.Instances
import Fmm.Launcher
import Fmm.Types
import Fmm.UI.Edit

import Control.Concurrent
import Control.Monad
import Data.Char (digitToInt, isAscii)
import Data.Text (Text)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.GObject
import GI.Gdk
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gio qualified as Gio
import GI.Gtk hiding (Text)
import System.Directory

css :: Text
css =
  " .launch { margin: 20px; border-radius: 20px; background-color: @accent_color; } \n\
  \ .launch * { color: @view_bg_color; } \n\
  \ .launch:active { background-color: lighter(@accent_color); } \n\
  \ .edit { margin: 20px; border-radius: 5px; margin-right: 0; } \n\
  \ .launcher dropdown, .launcher .download { margin: 5px; } \n\
  \ .row { margin: 10px; margin-bottom: 5px; border-radius: 10px; background-color: lighter(@view_bg_color); } \n\
  \ .moreMargin { margin-top: 12px; margin-bottom: 12px; } \n\
  \ * { outline: none; }"

addInstRow :: ListBox -> ApplicationWindow -> Instance -> IO ()
addInstRow list parent inst = do
  row <- Adw.actionRowNew
  Adw.actionRowActivate row
  Adw.preferencesRowSetTitle row (iname inst)
  Adw.actionRowSetSubtitle row (fVersion inst)
  widgetAddCssClass row "row"

  lb <- buttonNew
  buttonSetIconName lb "media-playback-start-symbolic"
  widgetAddCssClass lb "launch"

  st <- buttonNew
  buttonSetIconName st "emblem-system-symbolic"
  widgetAddCssClass st "edit"

  Adw.actionRowAddSuffix row st
  Adw.actionRowAddSuffix row lb

  !_ <- onButtonClicked lb (launchGame inst)
  !_ <- onButtonClicked st (spawnEditWindow inst list updateList parent)

  listBoxAppend list row

runGUI :: IO ()
runGUI = do
  Adw.init
  app <- applicationNew (Just "org.ProggerX.fmm") []

  !_ <- onApplicationActivate app (buildUI app)

  void $ applicationRun app Nothing

buildUI :: Application -> IO ()
buildUI app = do
  window <- applicationWindowNew app

  hb <- Adw.headerBarNew

  windowSetTitlebar window $ Just hb

  (Just display) <- displayGetDefault
  cssProvider <- cssProviderNew
  cssProviderLoadFromString cssProvider css
  styleContextAddProviderForDisplay display cssProvider 999

  list <- listBoxNew
  listBoxSetSelectionMode list SelectionModeNone

  instances <- getInstances

  mapM_ (addInstRow list window) instances

  listBoxAppend list =<< createNewRow list window

  windowSetChild window $ Just list
  windowPresent window

updateList :: ListBox -> ApplicationWindow -> IO ()
updateList list window = do
  listBoxRemoveAll list

  instances <- getInstances
  mapM_ (addInstRow list window) instances
  listBoxAppend list =<< createNewRow list window

createNewRow :: ListBox -> ApplicationWindow -> IO Adw.ButtonRow
createNewRow list parent = do
  r <- Adw.buttonRowNew

  Adw.preferencesRowSetTitle r "Add"
  Adw.buttonRowSetStartIconName r $ Just "list-add-symbolic"
  !_ <- Adw.onButtonRowActivated r $ do
    Adw.preferencesRowSetTitle r "Wait..."
    widgetSetSensitive r False
    void $ forkIO $ do
      vs <- concatMap createEditions <$> getVersions
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        makeNewInstance list r vs parent
        pure False

  pure r

createEditions :: Text -> [String]
createEditions t =
  map ((v ++ "-") ++) $ ["demo", "alpha", "headless"] ++ ["expansion" | digitToInt (hd v) > 1]
 where
  v = T.unpack t

makeNewInstance :: ListBox -> Adw.ButtonRow -> [String] -> ApplicationWindow -> IO ()
makeNewInstance list r vs parent = do
  listBoxRemove list r

  cr <- Adw.comboRowNew
  widgetAddCssClass cr "row"

  Adw.preferencesRowSetTitle cr ""

  m <- stringListNew $ Just $ map T.pack vs
  Adw.comboRowSetModel cr $ Just m

  entry <- entryNew
  widgetAddCssClass entry "moreMargin"
  entrySetPlaceholderText entry $ Just "Name"

  button <- buttonNewWithLabel "Create"
  widgetAddCssClass button "launch"

  !_ <- onButtonClicked button $ do
    txt <- entryBufferGetText =<< entryGetBuffer entry
    unless (null $ T.unpack txt) $ do
      Adw.actionRowRemove cr button
      sp <- spinnerNew
      Adw.actionRowAddSuffix cr sp
      spinnerStart sp
      lbl <- labelNew $ Just "Downloading..."
      Adw.actionRowAddSuffix cr lbl
      editableSetEditable entry False
      widgetSetSensitive cr False

      void $ forkIO $ do
        (Just vers) <- Adw.comboRowGetSelectedItem cr
        (Just t) <- castTo StringObject vers
        s <- T.unpack <$> stringObjectGetString t

        let v = T.pack $ takeWhile (/= '-') s
            e = T.pack $ drop 1 $ dropWhile (/= '-') s

        createInstance txt v e
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
          updateList list parent
          pure False

  local <- buttonNewWithLabel "Local..."
  widgetAddCssClass local "edit"
  !_ <-
    onButtonClicked local $ do
      fd <- fileDialogNew
      home <- getHomeDirectory
      f <- Gio.fileNewForPath $ home ++ "/.fmm/instances"

      fileDialogSetInitialFolder fd (Just f)

      fileDialogSelectFolder fd (Just parent) (Nothing @Gio.Cancellable) (Just $ localRespond fd)

      updateList list parent

  Adw.actionRowAddPrefix cr entry
  Adw.actionRowAddSuffix cr local
  Adw.actionRowAddSuffix cr button

  listBoxAppend list cr
 where
  localRespond :: FileDialog -> Maybe Object -> Gio.AsyncResult -> IO ()
  localRespond fd _ ar = do
    mf <- fileDialogSelectFolderFinish fd ar
    case mf of
      Just f -> do
        mp <- Gio.fileGetPath f
        case mp of
          Just p -> do
            putStrLn $ "User chose " ++ p
          Nothing -> pure ()
      Nothing -> pure ()
