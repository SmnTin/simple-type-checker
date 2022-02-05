module Main where
    
import Test.Tasty (defaultMain, testGroup)

import qualified SimpleTypeChecker.IO.Tests (tests)

main :: IO ()
main = defaultMain tests

tests = testGroup "SimpleTypeChecker.IO.Tests" SimpleTypeChecker.IO.Tests.tests