module Main where

import Control.Monad (forM_)
import GI.Types
import GI.Typelib
import GI.BaseInfo
import Graphics.UI.Gtk
import System.Glib.GError

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    tp <- load "TelepathyGLib" Nothing
    infos <- getInfos tp
    forM_ infos $ \info -> print (baseInfoName info, baseInfoType info)

