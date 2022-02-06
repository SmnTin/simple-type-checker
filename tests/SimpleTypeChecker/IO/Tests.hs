module SimpleTypeChecker.IO.Tests (tests) where

import Test.Tasty.HUnit
import Test.Tasty

tests = testGroup "SimpleTypeChecker.IO.Tests" [
            testCase "List comparison (different length)" $
                [1, 2, 3] `compare` [1,2] @?= GT
    ]