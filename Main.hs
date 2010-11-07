module Main where

import Control.Monad (forM_)
import GI.Types
import GI.Typelib
import GI.BaseInfo
import Graphics.UI.Gtk
import System.Glib.GError

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    tp <- repositoryRequire "TelepathyGLib"
    repositoryLoad tp
    n <- repositoryGetNInfos "TelepathyGLib"
    forM_ [0..n-1] $ \i -> do
        putStrLn =<< baseInfoGetName =<< repositoryGetInfo "TelepathyGLib" i


