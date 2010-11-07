module Main where

import GI.Typelib
import Graphics.UI.Gtk
import System.Glib.GError

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    tp <- repositoryRequire "GLib"
    repositoryLoad tp
    print =<< repositoryGetNInfos "GLib"
