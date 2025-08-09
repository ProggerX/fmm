{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI where

import Fmm.Instances
import Fmm.Types

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gdk
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gtk hiding (Text)

css :: Text
css =
  " .launch { margin: 20px; border-radius: 20px; background-color: @accent_color; } \n\
  \ .launch * { color: @view_bg_color; } \n\
  \ .launch:active { background-color: lighter(@accent_color); } \n\
  \ .launcher dropdown, .launcher .download { margin: 5px; } \n\
  \ * { outline: none; }"

addInstRow :: ListBox -> Instance -> IO ()
addInstRow list inst = do
  row <- Adw.actionRowNew
  Adw.preferencesRowSetTitle row (iname inst)
  Adw.actionRowActivate row
  Adw.actionRowSetSubtitle row (T.pack $ binPath inst)

  lb <- buttonNew
  buttonSetIconName lb "media-playback-start-symbolic"
  widgetAddCssClass lb "launch"

  Adw.actionRowAddSuffix row lb

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
