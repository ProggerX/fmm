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
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.GObject
import GI.Gdk
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gtk hiding (Text)
import System.IO

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

addInstRow :: ListBox -> Instance -> IO ()
addInstRow list inst = do
  ior <- newIORef inst
  row <- Adw.actionRowNew
  Adw.actionRowActivate row
  widgetAddCssClass row "row"

  lb <- buttonNew
  buttonSetIconName lb "media-playback-start-symbolic"
  widgetAddCssClass lb "launch"

  st <- buttonNew
  buttonSetIconName st "emblem-system-symbolic"
  widgetAddCssClass st "edit"

  Adw.actionRowAddSuffix row st
  Adw.actionRowAddSuffix row lb

  let ir = InstanceRow{row, lb, st}
  !_ <- onButtonClicked lb (launchGame =<< readIORef ior)
  !_ <- onButtonClicked st (spawnEditWindow ir ior)

  update ir ior
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

  mapM_ (addInstRow list) instances

  listBoxAppend list =<< createNewRow list

  windowSetChild window $ Just list
  windowPresent window

createNewRow :: ListBox -> IO Adw.ButtonRow
createNewRow list = do
  r <- Adw.buttonRowNew

  Adw.preferencesRowSetTitle r "Add"
  Adw.buttonRowSetStartIconName r $ Just "list-add-symbolic"
  !_ <- Adw.onButtonRowActivated r $ do
    !_ <- Adw.onButtonRowActivated r (pure ())
    void $ forkIO $ do
      vs <- concatMap createEditions <$> getVersions
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        makeNewInstance list r vs
        pure False

  pure r

createEditions :: Text -> [String]
createEditions t =
  map ((v ++ "-") ++) $ ["demo", "alpha", "headless"] ++ ["expansion" | digitToInt (hd v) > 1]
 where
  v = T.unpack t

makeNewInstance :: ListBox -> Adw.ButtonRow -> [String] -> IO ()
makeNewInstance list r vs = do
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
          listBoxRemoveAll list

          instances <- getInstances
          mapM_ (addInstRow list) instances
          listBoxAppend list =<< createNewRow list
          pure False

  Adw.actionRowAddPrefix cr entry
  Adw.actionRowAddSuffix cr button

  listBoxAppend list cr
