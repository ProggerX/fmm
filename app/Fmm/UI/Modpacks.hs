{-# LANGUAGE OverloadedStrings #-}

module Fmm.UI.Modpacks where

import Fmm.Modpacks

import Control.Monad (void)
import Data.Text
import GI.GObject
import GI.Gtk hiding (Text)
import System.Directory
import System.Process

buildModpacks :: Window -> IO Box
buildModpacks win = do
  hbox <- boxNew OrientationHorizontal 5

  list <- listBoxNew

  stringList <- getModpacks >>= stringListNew . Just
  listBoxBindModel list (Just stringList) (Just f)

  buttons <- boxNew OrientationVertical 40

  createButton <- buttonNewWithLabel "Zip mods folder to modpack"
  rmButton <- buttonNewWithLabel "Remove modpack"
  openButton <- buttonNewWithLabel "Open mods folder"
  engButton <- buttonNewWithLabel "Unzip modpack to mods folder"
  clsButton <- buttonNewWithLabel "Clear mods folder"

  !_ <- onButtonClicked createButton $ createModpack' list win
  !_ <- onButtonClicked engButton $ engageModpack' list
  !_ <- onButtonClicked openButton openFolder
  !_ <- onButtonClicked clsButton $ clearFolder win
  !_ <- onButtonClicked rmButton $ rmModpack list win

  boxAppend buttons createButton
  boxAppend buttons engButton
  boxAppend buttons clsButton
  boxAppend buttons openButton
  boxAppend buttons rmButton

  setWidgetVexpand buttons True
  setWidgetVexpand list True
  setWidgetHexpand list True
  setWidgetMarginTop buttons 40
  setWidgetMarginStart buttons 40
  setWidgetMarginEnd buttons 40

  boxAppend hbox list
  boxAppend hbox buttons
  pure hbox

f :: Object -> IO Widget
f a =
  toWidget =<< do
    (Just txt) <- castTo StringObject a
    stringObjectGetString txt >>= labelNew . Just

createModpack' :: ListBox -> Window -> IO ()
createModpack' list win = do
  dg <- windowNew
  windowSetModal dg True
  windowSetDefaultSize dg 300 100
  windowSetTransientFor dg $ Just win

  labelNew (Just "Enter modpack name") >>= windowSetTitlebar dg . Just

  box <- boxNew OrientationVertical 5
  setWidgetVexpand box True

  entry <- entryNew

  button <- buttonNewWithLabel "OK"
  !_ <- onButtonClicked button $ do
    buf <- entryGetBuffer entry
    entryBufferGetText buf >>= createModpack
    stringList <- getModpacks >>= stringListNew . Just
    listBoxBindModel list (Just stringList) (Just f)
    windowClose dg

  boxAppend box entry
  boxAppend box button

  windowSetChild dg $ Just box
  windowPresent dg

openFolder :: IO ()
openFolder = do
  home <- getHomeDirectory
  let path = home ++ "/.factorio/mods"
  void $ createProcess $ proc "/usr/bin/env" ["-S", "xdg-open", path]

clearFolder :: Window -> IO ()
clearFolder win = do
  dg <- windowNew
  windowSetModal dg True
  windowSetDefaultSize dg 700 50
  windowSetTransientFor dg $ Just win

  labelNew (Just "!!! FACTORIO MODS DIRECTORY WILL BE CLEARED IF YOU CLICK THIS BUTTON !!!") >>= windowSetTitlebar dg . Just

  button <- buttonNewWithLabel "OK"
  !_ <- onButtonClicked button $ do
    home <- getHomeDirectory
    let path = home ++ "/.factorio/mods"
    removeDirectoryRecursive path
    createDirectory path
    windowClose dg

  windowSetChild dg $ Just button
  windowPresent dg

engageModpack' :: ListBox -> IO ()
engageModpack' list = do
  mrow <- listBoxGetSelectedRow list
  case mrow of
    Just row -> do
      modpacks <- getModpacks
      i <- listBoxRowGetIndex row
      engageModpack $ modpacks !! fromEnum i
    Nothing -> pure ()

rmModpack :: ListBox -> Window -> IO ()
rmModpack list win = do
  dg <- windowNew
  windowSetModal dg True
  windowSetDefaultSize dg 700 50
  windowSetTransientFor dg $ Just win

  mrow <- listBoxGetSelectedRow list
  case mrow of
    Just row -> do
      modpacks <- getModpacks
      i <- listBoxRowGetIndex row

      let name = modpacks !! fromEnum i

      labelNew (Just $ mconcat ["!!! MODPACK \"", name, "\" WILL BE DELETED IF YOU CLICK THIS BUTTON !!!"]) >>= windowSetTitlebar dg . Just

      button <- buttonNewWithLabel "OK"
      !_ <- onButtonClicked button $ do
        home <- getHomeDirectory
        let path = home ++ "/.fmm/modpacks/" ++ unpack name
        removeFile path
        stringList <- getModpacks >>= stringListNew . Just
        listBoxBindModel list (Just stringList) (Just f)
        windowClose dg

      windowSetChild dg $ Just button
      windowPresent dg
    Nothing -> pure ()
