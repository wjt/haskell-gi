module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Graphics.UI.Gtk
import System.Glib.GError

import GI.API (loadAPI)
import GI.Code (codeToString, runCodeGen')
import GI.CodeGen (genModule)

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    [name] <- getArgs
    apis <- loadAPI name
    putStrLn $ codeToString $ runCodeGen' $ genModule name apis

