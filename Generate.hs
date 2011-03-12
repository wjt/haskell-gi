module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)

import qualified Data.Map as M

import Graphics.UI.Gtk
import System.Glib.GError

import GI.API (loadAPI)
import GI.Code (Config(..), codeToString, runCodeGen')
import GI.CodeGen (genModule)

main = handleGError (\(GError dom code msg) -> print (dom, code, msg)) $ do
    initGUI
    [name] <- getArgs
    apis <- loadAPI name
    let cfg = Config M.empty
    putStrLn $ codeToString $ runCodeGen' cfg  $ genModule name apis

