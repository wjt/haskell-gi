
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
import Data.Sequence (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as S

data CodeTag
    = Import
    | TypeDecl
    | Decl
    deriving (Eq, Show)

data Code
    = NoCode
    | Line String
    | Indent Code
    | Sequence (Seq Code)
    | Tag CodeTag Code
    deriving (Eq, Show)

instance Monoid Code where
    mempty = NoCode

    NoCode `mappend` NoCode = NoCode
    x `mappend` NoCode = x
    NoCode `mappend` x = x
    (Sequence a) `mappend` (Sequence b) = Sequence (a >< b)
    (Sequence a) `mappend` b = Sequence (a |> b)
    a `mappend` (Sequence b) = Sequence (a <| b)
    a `mappend` b = Sequence (a <| b <| S.empty)

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
          str n (Sequence s) cont = deseq n (S.viewl s) cont
          -- str n (Sequence s) cont = F.foldr (\code rest -> str n code : rest) cont s

          deseq _ S.EmptyL cont = cont
          deseq n (c :< cs) cont = str n c (deseq n (S.viewl cs) cont)

codeToList c = list c []
    where list NoCode cont = cont
          list (Sequence s) cont = F.foldr (:) cont s
          list c cont = c : cont

