
module GI.Code
    ( Code(..)
    , CodeTag(..)
    , CodeGen
    , Config(..)
    , runCodeGen
    , runCodeGen'
    , codeToString
    , codeToList
    , indent
    , line
    , blank
    , tag
    , config
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M

data CodeTag
    = Import
    | TypeDecl
    | Decl
    deriving Show

data Code
    = NoCode
    | Line String
    | Indent Code
    | Concat Code Code
    | Tag CodeTag Code
    deriving Show

instance Monoid Code where
    mempty = NoCode

    (Concat a b) `mappend` c = Concat a (Concat b c)
    a `mappend` b = Concat a b

data Config = Config {
  prefixes :: M.Map String String,
  names :: M.Map String String }

type CodeGen = WriterT Code (Reader Config)

runCodeGen :: Config -> CodeGen a -> (a, Code)
runCodeGen config = flip runReader config . runWriterT

runCodeGen' :: Config -> CodeGen () -> Code
runCodeGen' cfg = snd . runCodeGen cfg

recurse :: CodeGen a -> CodeGen (a, Code)
recurse cg = do
    cfg <- config
    return $ runCodeGen cfg cg

tag :: CodeTag -> CodeGen a -> CodeGen a
tag t cg = do
    (x, code) <- recurse cg
    tell $ Tag t code
    return x

line :: String -> CodeGen ()
line = tell . Line

blank = line ""

config :: CodeGen Config
config = lift ask

indent :: CodeGen a -> CodeGen a
indent cg = do
    (x, code) <- recurse cg
    tell $ Indent code
    return x

codeToString c = concatMap (++ "\n") $ str 0 c []
    where str _ NoCode cont = cont
          str n (Line s) cont = (replicate (n * 4) ' ' ++ s) : cont
          str n (Indent c) cont = str (n + 1) c cont
          str n (Tag _ c) cont = str n c cont
          str n (Concat c1 c2) cont = str n c1 (str n c2 cont)

codeToList c = list c []
    where list NoCode cont = cont
          list (Concat c1 c2) cont = list c1 (list c2 cont)
          list c cont = c : cont

