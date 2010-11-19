
module GI.Code
    ( Code(..)
    , CodeGen
    , runCodeGen
    , runCodeGen'
    , codeToString
    , indent
    , line
    , blank
    ) where

import Control.Monad.Writer

data Code
    = NoCode
    | Line String
    | Indent Code
    | Concat Code Code
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
          str n (Concat c1 c2) cont = str n c1 (str n c2 cont)

