module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Graphics.UI.Gtk
import System.Glib.GError

import GI.API
import GI.Code
import GI.CodeGen

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    [name] <- getArgs
    apis <- loadAPI name
    forM_ apis $ \api ->
        putStr . codeToString . runCodeGen' $ case api of
            APIConst c -> genConstant c >> line ""
            APIFunction f -> genFunction f >> line ""
            _ -> return ()

