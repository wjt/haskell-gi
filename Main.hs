module Main where

import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as M

--import Graphics.UI.Gtk
import System.Glib.Initialize
import System.Glib.GError

import GI.API (loadAPI)
import GI.Code (Config(..), codeToString, runCodeGen')
import GI.CodeGen (genModule)

data Mode = GenerateCode | Dump
  deriving Show

data Options = Options {
  optMode :: Mode,
  optRenames :: [(String, String)],
  optPrefixes :: [(String, String)] }
    deriving Show

defaultOptions = Options {
  optMode = GenerateCode,
  optRenames = [],
  optPrefixes = [] }

parseKeyValue s =
  let (a, ('=':b)) = break (=='=') s
   in (a, b)

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "d" ["dump"] (NoArg $ \opt -> opt { optMode = Dump })
    "dump internal representation instead of generating code",
  Option "p" ["prefix"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optPrefixes = (a, b) : optPrefixes opt }) "A=B")
     "specify the prefix for a particular namespace",
  Option "r" ["rename"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optRenames = (a, b) : optRenames opt }) "A=B")
    "specify a Haskell name for a C name"]

printGError = handleGError (\(GError _dom _code msg) -> putStrLn msg)

processAPI options name = do
    apis <- loadAPI name
    let cfg = Config {
          prefixes = M.fromList (optPrefixes options),
          names = M.fromList (optRenames options) }

    case optMode options of
        GenerateCode ->
            putStrLn $ codeToString $ runCodeGen' cfg $ genModule name apis
        Dump -> mapM_ print apis

main = printGError $ do
    args <- initArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder optDescrs args
        options  = foldl (.) id actions defaultOptions

    case errors of
        [] -> return ()
        _ -> do
            mapM_ (hPutStr stderr) errors
            exitFailure

    case nonOptions of
        [name] -> processAPI options name
        _ -> do
            hPutStrLn stderr "usage: haskell-gi [options] package"
            exitFailure
