{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI.Edit where

import Data.Text qualified as T
import Fmm.Instances
import Fmm.Types
import GI.Adw qualified as Adw
import GI.Gtk

spawnEditWindow :: Adw.ActionRow -> Instance -> IO ()
spawnEditWindow row inst = do
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
  !_ <- Adw.onEntryRowApply nmRow $ editableGetText nmRow >>= renameInstance inst
  listBoxAppend list nmRow
  widgetAddCssClass nmRow "row"

  windowSetChild win $ Just list
  windowSetTitlebar win $ Just hb
  windowPresent win
