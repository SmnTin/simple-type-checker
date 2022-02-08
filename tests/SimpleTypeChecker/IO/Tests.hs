module SimpleTypeChecker.IO.Tests (tests) where

import SimpleTypeChecker.IO
import SimpleTypeChecker.Types
import SimpleTypeChecker.Types.TestBindings
import SimpleTypeChecker.Parser

import Test.Tasty.HUnit
import Test.Tasty

tests = testGroup "SimpleTypeChecker.IO" [
        testParse
      , testShow
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
generateShowTests :: (Eq a, Show a) => TestName -> [ShowTest a] -> TestTree
generateShowTests testGroupName parseTests =
    testGroup testGroupName $ do
        ShowTest testName expected expr <- parseTests
        return $ testCase testName $ assertEqual "" expected $ show expr

testParseVariablesOnly = generateParseTests "Variables only" parseExpression [
        ParseTest "Simple"                      (Just $ Var "x")        "x"
      , ParseTest "Trailing spaces"             (Just $ Var "y")        "y   "
      , ParseTest "Trailing and leading spaces" (Just $ Var "y")        "  y   "
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
      , ParseTest "Empty type"
            (Just emptyT)
            "@a. a"
      , ParseTest "Forall quantifier spreads right"
            (Just boolT)
            "@a. a -> a -> a"
      , ParseTest "Nested quantifiers"
            (Just kCombT)
            "@a. @b. a -> b -> a"
      , ParseTest "Quantifier in brackets"
            (Just $ Forall "a" $ TVar "a" :-> Forall "b" (TVar "b") :-> TVar "a")
            "@a. a -> (@b. b) -> a"
    ]

testParseLambda = generateParseTests "Lambda" parseExpression [
        ParseTest "Simple"
            (Just $ Lam "x" (TVar "a") (Var "x"))
            "\\x : a. x"
      , ParseTest "Simple with leading spaces"
            (Just $ Lam "x" (TVar "a") (Var "x"))
            "  \\ x : a. x"
      , ParseTest "Argument with arrow type"
            (Just $ Lam "x" (TVar "a" :-> TVar "b") (Var "x"))
            "  \\ x : a -> b. x"
      , ParseTest "Argument with quintifier type"
            (Just $ Lam "x" (Forall "c" $ TVar "c") (Var "x"))
            "  \\ x : @c.c. x"
      , ParseTest "Nested"
            (Just $ Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x")))
            "\\x : a. \\y : b. x"
      , ParseTest "Nested with arrow types"
            (Just $ Lam "x" (TVar "a" :-> TVar "b") (Lam "y" (TVar "c" :-> TVar "d") (Var "x")))
            "\\x : (a -> b). \\y : (c -> d). x"
      , ParseTest "No type"
            Nothing "\\x -> x"
      , ParseTest "No dot"
            Nothing "\\x : a  x"
      , ParseTest "No type 2"
            Nothing "\\x :  . x"
      , ParseTest "Type lambda"
            (Just id')
            "#a. \\x : a. x"
      , ParseTest "Type lambda with spaces"
            (Just id')
            " #  a. \\  x  :   a  . x    "
      , ParseTest "Closed K-combinator"
            (Just kComb)
            "#a. #b. \\x : a. \\y : b. x"
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
      , ParseTest "Type application"
            (Just $ Var "f" :@* TVar "x")
            "f [x]"
      , ParseTest "Mixed applications"
            (Just $ Var "f" :@* TVar "a" :@ Var "g" :@* (TVar "b" :-> TVar "d") :@* TVar "c" :@ Var "h")
            "f [a] g [b -> d] [c] h"
      , ParseTest "First applicant is a type"
            Nothing
            "[x] f"
    ]

testParseComplex = generateParseTests "Complex" parseExpression [
        ParseTest "Lambda spreads right"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y"))
            "\\x : a. x y"
      , ParseTest "Type lambda spreads right"
            (Just $ TLam "a" $ Lam "x" (TVar "a") (Var "x") :@ Lam "y" (TVar "a") (Var "y"))
            "#a. (\\x : a. x) (\\y : a. y)"
      , ParseTest "Lambda in brackets"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y"))
            "(\\x : a. x y)"
      , ParseTest "Type lambda in brackets"
            (Just $ TLam "a" $ Lam "x" (TVar "a") (Var "x" :@ Var "y"))
            " (#a. (\\x : a. x y))"
      , ParseTest "Lambda application"
            (Just $ Lam "x" (TVar "a") (Var "x" :@ Var "y") :@ Var "z")
            "(\\x : a. x y) z"
      , ParseTest "Type application with lambda"
            (Just $ TLam "a" (Lam "x" (TVar "a") (Var "x" :@ Var "y")) :@* TVar "b")
            "(#a. \\x : a. x y) [b]"
      , ParseTest "Lambda application with long names"
            (Just $ Lam "x9row" (TVar "abba") (Var "x9" :@ Var "yzz'") :@ Var "z_ff")
            "(\\x9row : abba. x9 yzz') z_ff"
      , ParseTest "Complex"
            (Just $ Lam "z" (TVar "d") $
                    Lam "x" (TVar "a" :-> TVar "b") (Var "x" :@ Var "y" :@ Var "z")
                    :@ Var "t"
                    :@ Lam "y" (TVar "b") (Var "x" :@ Var "y"))
            "\\z : d. (\\x : (a -> b). x y z) t (\\y : b. x y)"
      , ParseTest "Brain destroyer"
            (Just $ TLam "b" (TLam "c" $ Lam "x" (TVar "b") $ Lam "y" (TVar "c") $ Var "x")
                    :@* Forall "a" (TVar "a" :-> TVar "c"))
            "(#b. #c. \\x:b. \\y:c. x) [@a. a -> c]"
    ]

testParseTypingRelation = generateParseTests "Typing relation" parseTypingRelation [
        ParseTest "Simple"
            (Just $ TypingRelation
                    (Env [("y", TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "y : b |- \\x : a. x y : a -> b -> a"
      , ParseTest "Empty expression"
            Nothing
            "y : b |- : a -> b -> a"
      , ParseTest "No type"
            Nothing
            "y : b |- \\x : a. x y : "
      , ParseTest "No type"
            Nothing
            "y : b |- \\x : a. x y"
      , ParseTest "Long environment"
            (Just $ TypingRelation
                    (Env [("y", TVar "b"), ("z", TVar "q")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "y : b, z : q |- \\x : a. x y : a -> b -> a"
      , ParseTest "No comma in environment"
            Nothing
            "y : b z : q |- \\x : a. x y : a -> b -> a"
      , ParseTest "Extra comma in environment"
            Nothing
            "y : b, z : q, |- \\x : a. x y : a -> b -> a"
      , ParseTest "Empty environment"
            (Just $ TypingRelation
                    (Env [])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "  |- \\x : a. x y : a -> b -> a"
      , ParseTest "Arrow types in environment"
            (Just $ TypingRelation
                    (Env [("z", TVar "a" :-> TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "z : a -> b |- \\x : a. x y : a -> b -> a"
      , ParseTest "Application"
            (Just $ TypingRelation
                    (Env [("z", TVar "a" :-> TVar "b")])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y")
                    :@ Lam "t" (TVar "r") (Var "t"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
            "z : a -> b |- (\\x : a. x y) (\\t : r. t) : a -> b -> a"
    ]

testShow = testGroup "Showing" [
        testShowExpression
      , testShowType
      , testShowTypingRelation
    ]

testShowExpression = generateShowTests "Expression" [
        ShowTest "Simple lambda"
            "\\x : a. x"
            (Lam "x" (TVar "a") (Var "x"))
      , ShowTest "Arrow type in lambda"
            "\\x : (a -> b). x"
            (Lam "x" (TVar "a" :-> TVar "b") (Var "x"))
      , ShowTest "Quantifier type in lambda"
            "\\x : (@c. c). x"
            (Lam "x" (Forall "c" $ TVar "c") (Var "x"))
      , ShowTest "K-combinator"
            "\\x : a. \\y : b. x"
            (Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x")))
      , ShowTest "Lambda spreads right"
            "\\x : a. x y"
            (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
      , ShowTest "Type lambda spreads right"
            "#a. x y"
            (TLam "a" (Var "x" :@ Var "y"))
      , ShowTest "Application"
            "x y"
            (Var "x" :@ Var "y")
      , ShowTest "Type application"
            "x [y]"
            (Var "x" :@* TVar "y")
      , ShowTest "Left associativity of application"
            "x y z"
            (Var "x" :@ Var "y" :@ Var "z")
      , ShowTest "Left associativity of application 2"
            "x (y z)"
            (Var "x" :@ (Var "y" :@ Var "z"))
      , ShowTest "Mixed applications"
            "f [a] g [b -> d] [c] h"
            (Var "f" :@* TVar "a" :@ Var "g" :@* (TVar "b" :-> TVar "d") :@* TVar "c" :@ Var "h")
      , ShowTest "Application with lambda"
            "(\\x : a. x y) (\\x : a. x)"
            (Lam "x" (TVar "a") (Var "x" :@ Var "y") :@ Lam "x" (TVar "a") (Var "x"))
      , ShowTest "Application with type lambda"
            "(#a. \\x : a. x) [a -> b]"
            (TLam "a" (Lam "x" (TVar "a") (Var "x")) :@* (TVar "a" :-> TVar "b"))
    ]

testShowType = generateShowTests "Type" [
        ShowTest "Single var"
            "x"
            (TVar "x")
      , ShowTest "Arrow"
            "x -> y"
            (TVar "x" :-> TVar "y")
      , ShowTest "Right associativity"
            "x -> y -> z"
            (TVar "x" :-> TVar "y" :-> TVar "z")
      , ShowTest "Right associativity 2"
            "(x -> y) -> z"
            ((TVar "x" :-> TVar "y") :-> TVar "z")
      , ShowTest "Brackets mid"
            "x -> (y -> t) -> z"
            (TVar "x" :-> (TVar "y" :-> TVar "t") :-> TVar "z")
      , ShowTest "Forall quantifier spreads right"
            "@a. a -> a -> a"
            boolT
      , ShowTest "Nested quantifiers"
            "@a. @b. a -> b -> a"
            kCombT
      , ShowTest "Quantifier in brackets"
            "@a. a -> (@b. b) -> a"
            (Forall "a" $ TVar "a" :-> Forall "b" (TVar "b") :-> TVar "a")
    ]


testShowTypingRelation = generateShowTests "Typing relation" [
        ShowTest "Simple"
            "y : b |- \\x : a. x y : a -> b -> a"
            (TypingRelation
                (Env [("y", TVar "b")])
                (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                (TVar "a" :-> TVar "b" :-> TVar "a"))
      , ShowTest "Empty environment"
            "|- \\x : a. x y : a -> b -> a"
            (TypingRelation
                    (Env [])
                    (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                    (TVar "a" :-> TVar "b" :-> TVar "a"))
      , ShowTest "Arrow types in environment"
            "z : a -> b |- \\x : a. x y : a -> b -> a"
            (TypingRelation
                (Env [("z", TVar "a" :-> TVar "b")])
                (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                (TVar "a" :-> TVar "b" :-> TVar "a"))
      , ShowTest "Quantifier types in environment"
            "z : @a. a -> b |- \\x : a. x y : a -> b -> a"
            (TypingRelation
                (Env [("z", Forall "a" $ TVar "a" :-> TVar "b")])
                (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                (TVar "a" :-> TVar "b" :-> TVar "a"))
      , ShowTest "Application"
            "z : a -> b |- (\\x : a. x y) (\\t : r. t) : a -> b -> a"
            (TypingRelation
                (Env [("z", TVar "a" :-> TVar "b")])
                (Lam "x" (TVar "a") (Var "x" :@ Var "y")
                :@ Lam "t" (TVar "r") (Var "t"))
                (TVar "a" :-> TVar "b" :-> TVar "a"))
      , ShowTest "Long environment"
            "y : b, z : q |- \\x : a. x y : a -> b -> a"
            (TypingRelation
                (Env [("y", TVar "b"), ("z", TVar "q")])
                (Lam "x" (TVar "a") (Var "x" :@ Var "y"))
                (TVar "a" :-> TVar "b" :-> TVar "a"))
    ]