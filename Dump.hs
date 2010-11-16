module Main where

import Control.Monad (forM_)
import GI.API
import Graphics.UI.Gtk
import System.Glib.GError

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    apis <- loadAPI "TelepathyGLib"
    forM_ apis $ \api -> print api

