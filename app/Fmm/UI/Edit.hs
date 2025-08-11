{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fmm.UI.Edit where

import Fmm.Instances
import Fmm.Types

import Data.IORef
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gtk

instance Updatable InstanceRow Instance where
  update :: InstanceRow -> IORef Instance -> IO ()
  update InstanceRow{row} ior = do
    Adw.preferencesRowSetTitle row . iname =<< readIORef ior
    Adw.actionRowSetSubtitle row . fVersion =<< readIORef ior

spawnEditWindow :: InstanceRow -> IORef Instance -> IO ()
spawnEditWindow ir ior = do
  inst <- readIORef ior
  let nm = iname inst

  win <- windowNew

  windowSetTitle win $ Just $ T.concat ["Edit instance: ", nm]

  hb <- Adw.headerBarNew

  list <- listBoxNew
  listBoxSetSelectionMode list SelectionModeNone

  -- Name
  nmRow <- Adw.entryRowNew
  Adw.preferencesRowSetTitle nmRow "Name"
  editableSetText nmRow (iname inst)
  Adw.entryRowSetShowApplyButton nmRow True
  !_ <-
    Adw.onEntryRowApply nmRow $
      editableGetText nmRow >>= \iname -> do
        renameInstance inst iname
        writeIORef ior $ inst{iname}
        windowSetTitle win $ Just $ T.concat ["Edit instance: ", iname]
        update ir ior

  let rows = [nmRow]
  mapM_ (listBoxAppend list) rows
  mapM_ (`widgetAddCssClass` "row") rows

  windowSetChild win $ Just list
  windowSetTitlebar win $ Just hb
  windowPresent win
