{-# LANGUAGE OverloadedStrings #-}

module Fmm where

import Fmm.UI.Launcher (buildLauncher)

import Control.Monad (void)
import GI.Adw qualified as Adw
import GI.Gio (applicationRun, onApplicationActivate)
import GI.Gtk

runApp :: IO ()
runApp = do
  app <- applicationNew (Just "org.ProggerX.fmm") []

  !_ <- onApplicationActivate app (buildUI app)

  void $ applicationRun app Nothing

buildUI :: Application -> IO ()
buildUI app = do
  window <- applicationWindowNew app

  hbar <- Adw.headerBarNew

  stack <- Adw.viewStackNew

  launcher <- buildLauncher

  mods <- boxNew OrientationVertical 20

  modpacks <- boxNew OrientationVertical 20

  lb3 <- labelNew $ Just "idkmp"
  boxAppend modpacks lb3

  !_ <- Adw.viewStackAddTitledWithIcon stack launcher Nothing "Launcher" "emblem-system-symbolic"
  !_ <- Adw.viewStackAddTitledWithIcon stack mods Nothing "Mods" "folder-documents-symbolic"
  !_ <- Adw.viewStackAddTitledWithIcon stack modpacks Nothing "Modpacks" "folder-symbolic"

  selector <- Adw.viewSwitcherNew
  Adw.viewSwitcherSetStack selector $ Just stack
  Adw.viewSwitcherSetPolicy selector Adw.ViewSwitcherPolicyWide
  Adw.headerBarSetTitleWidget hbar $ Just selector

  windowSetTitlebar window $ Just hbar
  windowSetChild window $ Just stack
  windowPresent window
