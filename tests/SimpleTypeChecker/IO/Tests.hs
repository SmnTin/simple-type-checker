module SimpleTypeChecker.IO.Tests (tests) where

import SimpleTypeChecker.IO
import SimpleTypeChecker.Types
import SimpleTypeChecker.Parser

import Test.Tasty.HUnit
import Test.Tasty
import Control.Applicative

tests = testGroup "SimpleTypeChecker.IO.Tests" [
        testParse
    ]

testParse = testGroup "Parsing" [
        testParseVariablesOnly
      , testParseType
      , testParseLambda
      , testParseApplication
      , testParseComplex
      , testParseTypingRelation
    ]

data ParseTest a = ParseTest TestName (Maybe a) String

generateParseTests :: (Eq a, Show a) => TestName -> Parser a -> [ParseTest a] -> TestTree
generateParseTests testGroupName parser parseTests =
    testGroup testGroupName $ do
        ParseTest testName expected strToParse <- parseTests
        let parsedExpr = runParserFully parser strToParse
        return $ testCase testName $ assertEqual "" expected parsedExpr

data ShowTest a = ShowTest TestName String a
generateShowTests :: (Eq a, Show a) => TestName -> Parser a -> [ParseTest a] -> TestTree
generateShowTests testGroupName parser parseTests =
    testGroup testGroupName $ do
        ParseTest testName expected strToParse <- parseTests
        let parsedExpr = runParserFully parser strToParse
        return $ testCase testName $ assertEqual "" expected parsedExpr

testParseVariablesOnly = generateParseTests "Variables only" parseExpression [
        ParseTest "Simple"                      (Just $ Var "x")        "x"
      , ParseTest "Trailing spaces"             (Just $ Var "y")        "y   "
      , ParseTest "Trailing and leading spaces" (Just $ Var "y")        "y   "
      , ParseTest "Several characters"          (Just $ Var "len")      "len"
      , ParseTest "AlphaNum"                    (Just $ Var "fuNe9T")   "fuNe9T"
      , ParseTest "Underscore"                  (Just $ Var "just_var") "just_var"
      , ParseTest "Apostrophe"                  (Just $ Var "x'")       "x'"
      , ParseTest "Apostrophes"                 (Just $ Var "x'''")     "x'''"
      , ParseTest "Starts with apostrophe"      Nothing                 "'x"
      , ParseTest "Starts with digit"           Nothing                 "0x"
      , ParseTest "Starts with underscore"      Nothing                 "_x"
      , ParseTest "Hyphen"                      Nothing                 "just-var"
    ]

testParseType = generateParseTests "Type" parseType [
        ParseTest "Single var"
            (Just $ TVar "x")
            "x"
      , ParseTest "Arrow"
            (Just $ TVar "x" :-> TVar "y")
            "x -> y"
      , ParseTest "Arrow without spaces"
            (Just $ TVar "x" :-> TVar "y")
            "x->y"
      , ParseTest "Right associativity"
            (Just $ TVar "x" :-> (TVar "y" :-> TVar "z"))
            "x -> y -> z"
      , ParseTest "Right associativity 2"
            (Just $ TVar "x" :-> TVar "y" :-> TVar "z")
            "x -> y -> z"
      , ParseTest "Brackets left"
            (Just $ (TVar "x" :-> TVar "y") :-> TVar "z")
            "(x -> y) -> z"
      , ParseTest "Brackets right"
            (Just $ TVar "x" :-> TVar "y" :-> TVar "z")
            "x -> (y -> z)"
      , ParseTest "Brackets mid"
            (Just $ TVar "x" :-> (TVar "y" :-> TVar "t") :-> TVar "z")
            "x -> (y -> t) -> z"
      , ParseTest "Mismatched brackets"
            Nothing
            "x -> (y -> t -> (z)"
      , ParseTest "Var around brackets"
            (Just $ TVar "x" :-> TVar "y")
            "x -> (y)"
      , ParseTest "Long names"
            (Just $ TVar "xyy'" :-> TVar "y_gg2")
            "xyy' -> y_gg2"
    ]

testParseLambda = generateParseTests "Lambda" parseExpression [
        ParseTest "Simple"
            (Just $ Lam "x" (TVar "a") (Var "x"))
            "\\x : a -> x"
      , ParseTest "Simple with leading spaces"
            (Just $ Lam "x" (TVar "a") (Var "x"))
            "  \\ x : a -> x"
      , ParseTest "Arrow type"
            (Just $ Lam "x" (TVar "a" :-> TVar "b") (Var "x"))
            "  \\ x : (a -> b) -> x"
      , ParseTest "Nested"
            (Just $ Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x")))
            "\\x : a -> \\y : b -> x"
      , ParseTest "Nested with arrow types"
            (Just $ Lam "x" (TVar "a" :-> TVar "b") (Lam "y" (TVar "c" :-> TVar "d") (Var "x")))
            "\\x : (a -> b) -> \\y : (c -> d) -> x"
      , ParseTest "No type"
            Nothing "\\x -> x"
      , ParseTest "No arrow"
            Nothing "\\x : a . x"
      , ParseTest "Empty type"
            Nothing "\\x :  -> x"
      , ParseTest "Arrow type not in brackets"
            Nothing "\\x : a -> b -> x"
    ]

testParseApplication = generateParseTests "Application" parseExpression [
        ParseTest "Two variables"
            (Just $ Var "x" :@ Var "y")
            "x y"
      , ParseTest "Two variables with trailing and leading spaces"
            (Just $ Var "x" :@ Var "y")
            " x  y   "
      , ParseTest "Left associativity"
            (Just $ Var "x" :@ Var "y" :@ Var "z")
            "x y z"
      , ParseTest "Brackets left"
            (Just $ Var "x" :@ Var "y" :@ Var "z")
            "(x y) z"
      , ParseTest "Brackets right"
            (Just $ Var "x" :@ (Var "y" :@ Var "z"))
            "x (y z)"
    ]

testParseComplex = generateParseTests "Complex" parseExpression [
        ParseTest "Lambda spreads right"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y"))
            "\\x : a -> x y"
      , ParseTest "Lambda in brackets"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y"))
            "(\\x : a -> x y)"
      , ParseTest "Lambda application"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y") :@ Var "z")
            "(\\x : a -> x y) z"
      , ParseTest "Lambda application with long names"
            (Just $ Lam "x9row" (TVar "abba") (Var "x9" :@ Var "yzz'") :@ Var "z_ff")
            "(\\x9row : abba -> x9 yzz') z_ff"
      , ParseTest "Complex"
            (Just $ Lam "z" (TVar "d") $
                    Lam "x" (TVar "a" :-> TVar "b") (Var "x" :@ Var "y" :@ Var "z")
                    :@ Var "t"
                    :@ Lam "y" (TVar "b") (Var "x" :@ Var "y"))
            "\\z : d -> (\\x : (a -> b) -> x y z) t (\\y : b -> x y)"
    ]

testParseTypingRelation = generateParseTests "Typing relation" parseTypingRelation [
        ParseTest "Simple"
            (Just $ TypingRelation 
                    (Env [("y", TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "y : b |- \\x : a -> x y : a -> b -> a"
      , ParseTest "Empty environment"
            (Just $ TypingRelation 
                    (Env [])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "  |- \\x : a -> x y : a -> b -> a"
      , ParseTest "Arrow types in environment"
            (Just $ TypingRelation 
                    (Env [("z", TVar "a" :-> TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "z : a -> b |- \\x : a -> x y : a -> b -> a"
      , ParseTest "Application"
            (Just $ TypingRelation 
                    (Env [("z", TVar "a" :-> TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y")
                    :@ Lam "t" (TVar "r") (Var "t"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "z : a -> b |- (\\x : a -> x y) (\\t : r -> t) : a -> b -> a"
    ]