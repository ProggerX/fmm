{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI where

import Fmm.Instances
import Fmm.Launcher
import Fmm.Types
import Fmm.UI.Edit

import Control.Monad
import Data.IORef
import Data.Text (Text)
import GI.Adw qualified as Adw
import GI.Gdk
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gtk hiding (Text)

css :: Text
css =
  " .launch { margin: 20px; border-radius: 20px; background-color: @accent_color; } \n\
  \ .launch * { color: @view_bg_color; } \n\
  \ .launch:active { background-color: lighter(@accent_color); } \n\
  \ .edit { margin: 20px; border-radius: 5px; margin-right: 0; } \n\
  \ .launcher dropdown, .launcher .download { margin: 5px; } \n\
  \ .row { margin: 10px; margin-bottom: 5px; border-radius: 10px; background-color: lighter(@view_bg_color); } \n\
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

  windowSetChild window $ Just list
  windowPresent window
