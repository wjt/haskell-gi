
module GI.CodeGen
    ( genConstant
    , genFunction
    , genCode
    , genModule
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Writer (tell)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, partition)
import Data.Typeable (TypeRep)
import qualified Data.Map as M

import GI.API
import GI.Code
import GI.Type
import GI.Value
import GI.Internal.ArgInfo

import qualified Language.Haskell.Exts as HS

nowhere = (HS.SrcLoc "" 0 0)

litInt :: Integral i
       => i
       -> HS.Exp
litInt = HS.Lit . HS.Int . fromIntegral

litFrac :: Real r
        => r
        -> HS.Exp
litFrac = HS.Lit . HS.Frac . toRational

litString :: String
          -> HS.Exp
litString = HS.Lit . HS.String

valueExp :: Value -> HS.Exp
valueExp VVoid = HS.Con $ HS.Special HS.UnitCon
valueExp (VBoolean x) = HS.Con $ HS.UnQual $ HS.Ident $ show x
valueExp (VInt8 x)     = litInt x
valueExp (VUInt8 x)    = litInt x
valueExp (VInt16 x)    = litInt x
valueExp (VUInt16 x)   = litInt x
valueExp (VInt32 x)    = litInt x
valueExp (VUInt32 x)   = litInt x
valueExp (VInt64 x)    = litInt x
valueExp (VUInt64 x)   = litInt x
valueExp (VFloat x)    = litFrac x
valueExp (VDouble x)   = litFrac x
valueExp (VGType x)    = litInt x
valueExp (VUTF8 x)     = litString x
valueExp (VFileName x) = litString x

padTo n s = s ++ replicate (n - length s) ' '

split :: Char
      -> String
      -> [String]
split c s = split' s "" []
    where split' [] w ws = reverse (reverse w : ws)
          split' (x:xs) w ws =
              if x == c then split' xs "" (reverse w:ws)
                  else split' xs (x:w) ws

escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved s = s

ucFirst (x:xs) = toUpper x : map toLower xs
ucFirst "" = error "ucFirst: empty string"

ucFirst_ "" = "_"
ucFirst_ xs = ucFirst xs

getPrefix :: String -> CodeGen String
getPrefix ns = do
    cfg <- config
    case M.lookup ns (prefixes cfg) of
        Just p -> return p
        Nothing -> error $
            "no prefix defined for namespace " ++ show ns

mangleName :: ([String] -> [String])
           -> Named a
           -> CodeGen String
mangleName mangleParts (Named ns s _) = do
    cfg <- config

    case M.lookup s (names cfg) of
        Just s' -> return s'
        Nothing -> do
          let ss = split '_' s
          ss' <- addPrefix ss
          return . concat . mangleParts $ ss'
  where
    addPrefix ss = do
        prefix <- getPrefix ns
        return $ prefix : ss

lowerName :: Named a
          -> CodeGen String
lowerName = mangleName rename
  where
    rename [w] = [map toLower w]
    rename (w:ws) = map toLower w : map ucFirst_ ws
    rename [] = error "rename: empty list"

upperName :: Named a
          -> CodeGen String
upperName = mangleName (map ucFirst_)

haskellType' :: Type -> CodeGen TypeRep
haskellType' (TInterface ns n) = do
    prefix <- getPrefix ns
    return $ haskellType (TInterface "!!!" (ucFirst prefix ++ n))
haskellType' t = return $ haskellType t

foreignType' :: Type -> CodeGen TypeRep
foreignType' (TInterface ns n) = do
    prefix <- getPrefix ns
    return $ foreignType (TInterface undefined (ucFirst prefix ++ n))
foreignType' t = return $ foreignType t

prime = (++ "'")

mkLet name value = line $ "let " ++ name ++ " = " ++ value

mkBind name value = line $ name ++ " <- " ++ value

-- When constructing a source tree from scratch (rather than loading it with
-- parseWithComments), it's not currently possible to pretty-print with
-- comments. Language.Haskell.Exts.Annotated.ExactPrint can do it, but we can't
-- generate an exact tree for it to print.
--
-- This hack, suggested by Neil Mitchell, attaches an ANN pragma to a symbol
-- which just contains an arbitrary string. If necessary, we can later
-- post-process this into a comment.
annotateName :: HS.Name
             -> String
             -> CodeGen ()
annotateName name comment = do
    let annotation = HS.Lit (HS.String comment)
    decl $ HS.AnnPragma nowhere (HS.Ann name annotation)

genConstant :: Named Constant -> CodeGen ()
genConstant n@(Named _ _ (Constant value)) = do
    name' <- lowerName n
    let name'' = HS.Ident name'
    ht <- haskellType' $ valueType value
    annotateName name'' ("constant ++ name")
    let sig = HS.TyCon (HS.UnQual (HS.Ident (show ht)))
    decl $ HS.TypeSig nowhere [name''] sig
    decl $ HS.FunBind [HS.Match nowhere name'' [] Nothing
                       (HS.UnGuardedRhs (valueExp value))
                       (HS.BDecls [])
                      ]

foreignImport :: String -> Callable -> CodeGen ()
foreignImport symbol callable = tag Import $ do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        line =<< last
    where
    first = "foreign import ccall \"" ++ symbol ++ "\" " ++
                symbol ++ " :: "
    fArgStr arg = do
        ft <- foreignType' $ argType arg
        let start = show ft ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    last = show <$> io <$> foreignType' (returnType callable)

hToF :: Arg -> CodeGen ()
hToF arg = do
    hType <- haskellType' $ argType arg
    fType <- foreignType' $ argType arg
    if hType == fType
        then mkLet' "id"
        else hToF' (show hType) (show fType)

    where
    name = escapeReserved $ argName arg
    mkLet' conv = mkLet (prime name) (conv ++ " " ++ name)
    mkBind' conv = mkBind (prime name) (conv ++ " `fmap` " ++ name)
    hToF' "[Char]" "CString" = mkBind' "newCString"
    hToF' "Word"   "GType"   = mkLet' "fromIntegral"
    hToF' "Bool"   "CInt"    = mkLet' "fromEnum"
    hToF' "[Char]" "Ptr CString" = mkBind' "bullshit"
    hToF' hType fType = error $
        "don't know how to convert " ++ hType ++ " to " ++ fType

genCallable :: String -> Named Callable -> CodeGen ()
genCallable symbol n@(Named _ _ callable) = do
    foreignImport symbol callable
    wrapper

    where
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    wrapper = do
        let argName' = escapeReserved . argName
        name <- lowerName n
        tag TypeDecl signature
        tag Decl $ line $
            name ++ " " ++
            intercalate " " (map argName' inArgs) ++
            " = do"
        indent $ do
            convertIn
            line $ "result <- " ++ symbol ++
                concatMap (prime . (" " ++) . argName') (args callable)
            convertOut
    signature = do
        name <- lowerName n
        line $ name ++ " ::"
        indent $ do
            mapM_ (\a -> line =<< hArgStr a) inArgs
            result >>= line
    convertIn = forM_ (args callable) $ \arg -> do
        ft <- foreignType' $ argType arg
        if direction arg == DirectionIn
            then hToF arg
            else mkBind (prime $ argName arg) $
                     "malloc :: " ++
                     show (io $ ptr ft)
    -- XXX: Should create ForeignPtrs for pointer results.
    -- XXX: Check argument transfer.
    convertOut = do
        -- XXX: Do reverse conversion here.
        mkLet "result'" "result"
        forM_ outArgs $ \arg ->
            if direction arg == DirectionIn
                then return ()
                else do
                    mkBind (prime $ prime $ argName arg) $
                        "peek " ++ (prime $ argName arg)
        let pps = map (prime . prime . argName) outArgs
        out <- outType
        case (show out, outArgs) of
            ("()", []) -> line $ "return ()"
            ("()", _) -> line $ "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return result'"
            (_ , _) -> line $
                "return (" ++ intercalate ", " ("result'" : pps) ++ ")"
    hArgStr arg = do
        ht <- haskellType' $ argType arg
        let start = show ht ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    result = show <$> io <$> outType
    outType = do
        hReturnType <- haskellType' $ returnType callable
        hOutArgTypes <- mapM (haskellType' . argType) outArgs
        let justType = case outArgs of
                [] -> hReturnType
                _ -> "(,)" `con` (hReturnType : hOutArgTypes)
            maybeType = "Maybe" `con` [justType]
        return $ if returnMayBeNull callable then maybeType else justType

genFunction :: Function -> CodeGen ()
genFunction (Function symbol callable) = do
  line $ "-- function " ++ symbol
  genCallable symbol callable

genStruct :: (Named Struct) -> CodeGen ()
genStruct n@(Named _ name (Struct _fields)) = do
  line $ "-- struct " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"
  -- XXX: Generate code for fields.

camelize :: String -> String
camelize "" = ""
camelize s = case break (== '_') s of
    (firstPart, rest) -> ucFirst firstPart ++ camelize (drop 1 rest)

-- | Defines one clause of a function binding, which may match many arguments
match :: String   -- ^ function name
      -> [HS.Pat] -- ^ argument pattern matches
      -> HS.Exp   -- ^ the right hand side of the definition
      -> HS.Match -- ^ the resulting function binding clause
match fun patterns exp = HS.Match loc name patterns type_ rhs binds
  where
    loc = nowhere
    name = HS.Ident fun
    type_ = Nothing
    rhs = HS.UnGuardedRhs exp
    binds = HS.BDecls []

-- | Defines a wildcard pattern match which raises an error; for instance,
--   'wildcardError "Blah" foo' yields something like
--
--   foo x = error $ "Blah foo: unhandled value " ++ show x
wildcardError :: String    -- ^ context for the error message (eg. type name)
              -> String    -- ^ function name
              -> HS.Match  -- ^ the resulting function binding clause
wildcardError context fun =
    match fun [HS.PVar x] exp
  where
    x = HS.Ident "x"
    varExp = HS.Var . HS.UnQual
    error_ = varExp (HS.Ident "error")
    message = HS.Lit (HS.String (context ++ " " ++ fun ++ ": unhandled value "))
    showVar = HS.App (varExp (HS.Ident "show"))
                     (varExp x)
    fullMessage = HS.InfixApp message
                              (HS.QVarOp (HS.UnQual (HS.Symbol "++")))
                              showVar
    exp = HS.App error_ fullMessage

-- | Generates a simple instance declaration for a type of kind *.
instance_ :: String        -- ^ class name
          -> HS.Name       -- ^ type name
          -> [HS.InstDecl] -- ^ body of instance declaration
          -> HS.Decl       -- ^ resulting instance declaration
instance_ class_ name body = HS.InstDecl loc context className typeNames body
  where
    loc = nowhere
    context = []
    className = HS.UnQual (HS.Ident class_)
    typeNames = [HS.TyCon (HS.UnQual name)]


genEnumEsque :: String -> Named Enumeration -> CodeGen ()
genEnumEsque kind n@(Named _ name (Enumeration fields)) = do
  name' <- upperName n
  let name'' = HS.Ident name'

  -- data name''
  let mangledFields = map (\(n, v) -> (name' ++ camelize n, v)) fields
  let constructors =
          [ HS.QualConDecl nowhere [] [] (HS.ConDecl (HS.Ident n) [])
          | (n, _) <- mangledFields
          ]
  annotateName name'' (kind ++ " " ++ name)
  decl $ HS.DataDecl nowhere HS.DataType [] name'' [] constructors []

  -- instance Enum name''
  let fromEnumDecl = HS.InsDecl $ HS.FunBind
          [ match "fromEnum"
                   [HS.PApp (HS.UnQual (HS.Ident n)) []]
                   (litInt v)
          | (n, v) <- mangledFields
          ]
      toEnumDecl = HS.InsDecl . HS.FunBind $
          [ match "toEnum"
                  [HS.PLit . HS.Int . fromIntegral $ v]
                  (HS.Var (HS.UnQual (HS.Ident n)))
          | (n, v) <- mangledFields
          ] ++
          [ wildcardError name' "toEnum" ]

  decl $ instance_ "Enum" name'' [ fromEnumDecl, toEnumDecl ]

  -- instance Eq name''
  -- FIXME: well what should we do if enums have two fields with the same value?
  let eqOp = HS.UnQual (HS.Symbol "==")
      eqPat = HS.PInfixApp (HS.PVar (HS.Ident "x"))
                           eqOp
                           (HS.PVar (HS.Ident "y"))
      fromEnum_ x = HS.App (HS.Var (HS.UnQual (HS.Ident "fromEnum")))
                           (HS.Var (HS.UnQual (HS.Ident x)))
      eqBody = HS.InfixApp (fromEnum_ "x") (HS.QVarOp eqOp) (fromEnum_ "y")

  decl $ instance_ "Eq" name''
                   [ HS.InsDecl (HS.PatBind nowhere eqPat Nothing
                                            (HS.UnGuardedRhs eqBody)
                                            (HS.BDecls [])
                                )
                   ]

  -- instance Ord name''
  let compareBody =
          match "compare" []
                (HS.App (HS.Var (HS.UnQual (HS.Ident "comparing")))
                        (HS.Var (HS.UnQual (HS.Ident "fromEnum")))
                )
  decl $ instance_ "Ord" name'' [HS.InsDecl (HS.FunBind [compareBody])]

genEnum = genEnumEsque "enum"

genFlags :: Named Flags -> CodeGen ()
genFlags (Named ns name (Flags e)) = do
    genEnumEsque "flags" (Named ns name e)

genCallback :: Callback -> CodeGen ()
genCallback (Callback (Named _ name _)) = do
  line $ "-- callback " ++ name
  -- XXX
  line $ "type " ++ name ++ " =  () -> ()"

genUnion :: Named Union -> CodeGen ()
genUnion n@(Named _ name _) = do
  line $ "-- union " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " where"
  -- XXX: Generate code for fields.

genCode :: API -> CodeGen ()
genCode (APIConst c) = genConstant c >> blank
genCode (APIFunction f) = genFunction f >> blank
genCode (APIEnum e) = genEnum e >> blank
genCode (APIFlags f) = genFlags f >> blank
genCode (APICallback c) = genCallback c >> blank
genCode (APIStruct s) = genStruct s >> blank
genCode (APIUnion s) = genUnion s >> blank
genCode a = error $ "can't generate code for " ++ show a

genModule :: String -> [API] -> CodeGen ()
genModule name apis = do
    let importedModules = [ "Data.Int"
                          , "Data.Word"
                          , "Data.Ord"
                          , "Foreign"
                          , "Foreign.C"
                          ]
        imports = map (\n -> HS.ImportDecl nowhere (HS.ModuleName n) False False
                                           Nothing Nothing Nothing)
                      importedModules

    let m = HS.Module nowhere
                      (HS.ModuleName name)
                      [HS.LanguagePragma nowhere
                                         [HS.Ident "ForeignFunctionInterface"]]
                      Nothing
                      Nothing
                      imports
                      []

    line $ HS.prettyPrint m
    blank
    cfg <- config
    let (imports, rest) = splitImports $ runCodeGen' cfg $ forM_ apis genCode
    mapM_ (\c -> tell c >> blank) imports
    mapM_ tell rest

    where splitImports = partition isImport . codeToList
          isImport (Tag Import _code) = True
          isImport _ = False
