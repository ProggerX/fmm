{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fmm.UI.Edit where

import Fmm.Instances
import Fmm.Types

import Control.Concurrent
import Control.Monad
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gio
import GI.Gtk
import System.Directory

spawnEditWindow :: Instance -> ListBox -> (ListBox -> ApplicationWindow -> IO ()) -> ApplicationWindow -> IO ()
spawnEditWindow inst list' updateList parent = do
  win <- windowNew
  windowSetTransientFor win $ Just parent

  windowSetTitle win $ Just $ T.concat ["Edit instance: ", iname inst]

  hb <- Adw.headerBarNew

  list <- listBoxNew
  listBoxSetSelectionMode list SelectionModeNone

  -- Name
  nmRow <- Adw.entryRowNew
  widgetAddCssClass nmRow "row"
  Adw.preferencesRowSetTitle nmRow "Name"
  editableSetText nmRow (iname inst)
  Adw.entryRowSetShowApplyButton nmRow True
  !_ <-
    Adw.onEntryRowApply nmRow $
      editableGetText nmRow >>= \iname -> do
        renameInstance inst iname
        windowSetTitle win $ Just $ T.concat ["Edit instance: ", iname]
        updateList list' parent

  dlRow <- Adw.buttonRowNew
  widgetAddCssClass dlRow "row"
  widgetAddCssClass dlRow "destructive-action"
  Adw.preferencesRowSetTitle dlRow "Delete instance"
  !_ <-
    Adw.onButtonRowActivated dlRow $ do
      ad <- Adw.alertDialogNew (Just "Are you sure?") (Just $ T.concat ["Delete ", iname inst, "?"])
      Adw.alertDialogAddResponse ad "no" "Cancel"
      Adw.alertDialogAddResponse ad "yes" "Delete"
      Adw.alertDialogSetCloseResponse ad "no"
      Adw.alertDialogSetDefaultResponse ad $ Just "no"
      Adw.alertDialogChoose ad (Just win) (Nothing @Cancellable) (Just $ dialogEnd dlRow win ad)

  listBoxAppend list nmRow
  listBoxAppend list dlRow

  windowSetChild win $ Just list
  windowSetTitlebar win $ Just hb
  windowPresent win
 where
  dialogEnd :: Adw.ButtonRow -> Window -> Adw.AlertDialog -> Maybe GObject.Object -> AsyncResult -> IO ()
  dialogEnd dlRow win ad _ ar = do
    ans <- Adw.alertDialogChooseFinish ad ar
    case ans of
      "yes" -> do
        Adw.preferencesRowSetTitle dlRow "Deleting..."
        widgetSetSensitive dlRow False
        void $ forkIO $ do
          removeDirectoryRecursive $ ipath inst
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
            updateList list' parent
            windowClose win
            pure False
      _ -> pure ()
