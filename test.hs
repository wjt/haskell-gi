
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map as M
import qualified Data.Sequence as S

import GI.API
import GI.Code
import GI.CodeGen
import GI.Internal.ArgInfo
import GI.Type
import GI.Value

testConfig = Config {
  prefixes = M.fromList [("test", "test")],
  names = M.empty }

testCodeGen api code = (codeToList $ runCodeGen' testConfig $ genCode api) @?= code

testConstant = testCase "constant" $ testCodeGen
  (APIConst (Named "test" "foo" (Constant (VInt8 42))))
  [ Line "-- constant foo"
  , Line "testFoo :: Int8"
  , Line "testFoo = 42"
  , Line ""]

testFunction = testCase "function" $ testCodeGen
  (APIFunction $ Function {
      fnSymbol = "foo_bar",
      fnCallable = Named "test" "foo_bar" (
        Callable {
           returnType = TBasicType TVoid,
           returnMayBeNull = False,
           returnTransfer = TransferNothing,
           returnAttributes = [],
           args = [
             Arg {
                argName = "x",
                argType = TBasicType TInt8,
                direction = DirectionIn,
                transfer = TransferNothing,
                scope = ScopeTypeInvalid }]})})
  [ Line "-- function foo_bar"
  , Tag Import $ Sequence $ S.fromList
      [ Line "foreign import ccall \"foo_bar\" foo_bar :: "
      , Indent $ Sequence $ S.fromList
        [ Line "Int8 ->                                 -- x"
        , Line "IO ()"
        ]
      ]
  , Tag TypeDecl $ Sequence $ S.fromList
      [ Line "testFooBar ::"
      , Indent $ Sequence $ S.fromList
        [ Line "Int8 ->                                 -- x"
        , Line "IO ()"
        ]
      ]
  , Tag Decl $ Line "testFooBar x = do"
  , Indent $ Sequence $ S.fromList
    [ Line "let x' = id x"
    , Line "result <- foo_bar x'"
    , Line "let result' = result"
    , Line "return ()"
    ]
  , Line ""
  ]

testEnum = testCase "enum" $ testCodeGen code expected
  where
    enum = Enumeration [("foo", 1), ("bar", 2)]
    code = APIEnum $ Named "test" "enum" enum
    expected =
      [ Line "-- enum enum"
      , Line "data TestEnum = "
      , Indent $ Sequence $ S.fromList
        [Line "  TestTestEnumFoo",
         Line "| TestTestEnumBar"
        ]
      , Line ""
      , Line "instance Enum TestEnum where"
      , Indent $ Sequence $ S.fromList
        [Line "fromEnum TestEnumFoo = 1",
         Line "fromEnum TestEnumBar = 2"]
      , Line ""
      , Indent $ Sequence $ S.fromList
        [Line "toEnum 1 = TestEnumFoo"
        , Line "toEnum 2 = TestEnumBar"]
      , Line ""]

main = defaultMain [
  testConstant,
  testFunction,
  testEnum]
