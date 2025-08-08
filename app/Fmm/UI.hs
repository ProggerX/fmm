{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI where

import Fmm.Instances
import Fmm.Types

import Control.Monad
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gtk

addInstRow :: ListBox -> Instance -> IO ()
addInstRow list inst = do
  row <- Adw.actionRowNew
  Adw.preferencesRowSetTitle row (T.pack $ binPath inst)
  Adw.actionRowActivate row
  Adw.actionRowSetSubtitle row (T.pack $ modsPath inst)

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

  list <- listBoxNew

  instances <- getInstances

  mapM_ (addInstRow list) instances

  windowSetChild window $ Just list
  windowPresent window
