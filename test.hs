
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

main = defaultMain [
  testConstant]
