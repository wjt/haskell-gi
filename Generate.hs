module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Graphics.UI.Gtk
import System.Glib.GError

import GI.API
import GI.CodeGen

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    [name] <- getArgs
    apis <- loadAPI name
    forM_ apis $ \api ->
        case api of
            APIConst c -> putStrLn $ genConstant c
            APIFunction f -> putStrLn $ genFunction f
            _ -> return ()

