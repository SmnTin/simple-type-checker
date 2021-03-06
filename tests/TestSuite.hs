module Main where
    
import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified SimpleTypeChecker.IO.Tests (tests)
import qualified SimpleTypeChecker.TypeChecker.Tests (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree 
tests = testGroup "Tests" [ SimpleTypeChecker.IO.Tests.tests
                          , SimpleTypeChecker.TypeChecker.Tests.tests
                          ]