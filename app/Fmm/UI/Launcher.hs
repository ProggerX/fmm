{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Fmm.UI.Launcher where

import Fmm.Launcher

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFileRelative)
import Data.Text
import GI.Adw qualified as Adw
import GI.Gdk
import GI.Gio qualified as Gio
import GI.Gtk hiding (Text)

css :: Text
css =
  " .launch { padding: 10px 30px; border-radius: 20px; background-color: @accent_color; } \n\
  \ .launch * { color: @view_bg_color; } \n\
  \ .launch:active { background-color: lighter(@accent_color); } \n\
  \ .launcher dropdown, .launcher .download { margin: 5px; } \n\
  \ * { outline: none; }"

fullLogo :: BS.ByteString
fullLogo = $(embedFileRelative "./assets/press-kit/logos/factorio-logo.png")

wheelLogo :: BS.ByteString
wheelLogo = $(embedFileRelative "./assets/press-kit/logos/factorio-logo.png")

buildLauncher :: IO Box
buildLauncher = do
  box <- boxNew OrientationVertical 100
  widgetAddCssClass box "launcher"

  hbox <- boxNew OrientationHorizontal 22

  BS.writeFile "/tmp/logo.png" fullLogo

  img <- imageNew
  imageSetFromFile img (Just "/tmp/logo.png")
  widgetSetVexpand img True

  setWidgetValign hbox AlignEnd
  setWidgetHalign hbox AlignCenter

  versions <- getVersions
  dropDown <- dropDownNewFromStrings versions

  lbox <- boxNew OrientationHorizontal 10
  imageNewFromIconName (Just "media-playback-start-symbolic") >>= boxAppend lbox
  labelNew (Just "Launch") >>= boxAppend lbox

  launchButton <- buttonNew
  widgetAddCssClass launchButton "launch"
  buttonSetChild launchButton $ Just lbox
  !_ <- onButtonClicked launchButton (launchGame' dropDown versions)

  rbox <- boxNew OrientationHorizontal 5
  labelNew (Just "Version:") >>= boxAppend rbox
  boxAppend rbox dropDown

  boxAppend hbox rbox
  boxAppend hbox launchButton

  setWidgetMarginBottom hbox 15

  (Just display) <- displayGetDefault
  cssProvider <- cssProviderNew
  cssProviderLoadFromString cssProvider css
  styleContextAddProviderForDisplay display cssProvider 999

  -- downloadButton <- buttonNewWithLabel "Download more versions"
  -- widgetAddCssClass downloadButton "download"
  -- !_ <- onButtonClicked downloadButton downloadGame'
  --
  -- boxAppend hbox downloadButton

  boxAppend box img
  boxAppend box hbox
  pure box

launchGame' :: DropDown -> [Text] -> IO ()
launchGame' dd versions = do
  i <- dropDownGetSelected dd
  launchGame $ unpack $ versions !! fromEnum i

showAlert :: Adw.AlertDialog -> IO ()
showAlert dg = Adw.alertDialogChoose dg (Nothing @Window) (Nothing @Gio.Cancellable) Nothing

-- downloadGame' :: IO ()
-- downloadGame' = do
--   dg <- Adw.alertDialogNew (Just "Warning") (Just "You'll need to restart the launcher to see downloaded versions")
--   Adw.alertDialogAddResponse dg "ok" "OK"
--   showAlert dg
