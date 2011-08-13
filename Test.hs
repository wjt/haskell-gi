
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map as M

import GI.API
import GI.Code
import GI.CodeGen
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

testEnum = testCase "enum" $ testCodeGen
    (APIEnum (Named "test" "foo" (Enumeration fields))) $
    [ Line "-- enum foo"
    , Line "data TestFoo ="
    , Indent (
        Concat (Line "  TestFooBarBaz")
               (Line "| TestFooSquidCatBattle")
      )
    , Line ""
    , Line "instance Enum TestFoo where"
    , Indent (
        Concat (Line "fromEnum TestFooBarBaz = 51")
               (Line "fromEnum TestFooSquidCatBattle = 75")
      )
    , Line ""
    , Indent (
        Concat (Line "toEnum 51 = TestFooBarBaz")
               (Concat (Line "toEnum 75 = TestFooSquidCatBattle")
                       (Line "toEnum n = error $ \"bad value \" ++ show n ++ \" for enum TestFoo\"")
               )
      )
    , Line ""
    , Line "instance Eq TestFoo where"
    , Indent (Line "x == y = fromEnum x == fromEnum y")
    , Line ""
    , Line "instance Ord TestFoo where"
    , Indent (Line "compare x y = compare (fromEnum x) (fromEnum y)")
    , Line ""
    ]
  where
    fields = [("BAR_BAZ", 51)
             ,("SQUID_CAT_BATTLE", 75)
             ]



main = defaultMain
    [ testConstant
    , testEnum
    ]
