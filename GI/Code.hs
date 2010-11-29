
module GI.Code
    ( Code(..)
    , CodeTag(..)
    , CodeGen
    , runCodeGen
    , runCodeGen'
    , codeToString
    , codeToList
    , indent
    , line
    , blank
    , tag
    ) where

import Control.Monad.Writer

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

type CodeGen = Writer Code

runCodeGen :: CodeGen a -> (a, Code)
runCodeGen = runWriter

runCodeGen' :: CodeGen () -> Code
runCodeGen' = snd . runCodeGen

tag :: CodeTag -> CodeGen () -> CodeGen ()
tag t = tell . Tag t . runCodeGen'

line :: String -> CodeGen ()
line = tell . Line

blank = line ""

indent :: CodeGen a -> CodeGen a
indent cg = let (x, c) = runWriter cg
             -- in CodeGen (x, Indent c)
             in tell (Indent c) >> return x

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

