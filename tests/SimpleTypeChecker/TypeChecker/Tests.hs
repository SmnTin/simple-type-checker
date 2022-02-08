module SimpleTypeChecker.TypeChecker.Tests (tests) where

import SimpleTypeChecker.Types
import SimpleTypeChecker.TypeChecker
import SimpleTypeChecker.IO
import Control.Monad.Except

import Test.Tasty.HUnit
import Test.Tasty
import SimpleTypeChecker.TypeChecker


tests = testGroup "SimpleTypeChecker.TypeChecker" [
        testEnvExtension
      , testAlphaEq
      , testInferringVariableType
      , testInferringLambdaType
      , testInferringTypeLambdaType
      , testInferringApplicationType
      , testInferringTypeApplicationType
      , testInferringTypeComplex
    ]

testEnvExtension = testGroup "Environment extension" [
        testCase "Variable is already present" $
            let given = Env [("x", TVar "a"), ("y", TVar "b"), ("z", TVar "c")]
                expected = Env [("x", TVar "a"), ("y", TVar "b" :-> TVar "d"), ("z", TVar "c")]
                actual = extendEnv given "y" $ TVar "b" :-> TVar "d"
            in assertEqual "" expected actual
      , testCase "Variable is not present" $
            let given = Env []
                expected = Env [("x", TVar "a")]
                actual = extendEnv given "x" $ TVar "a"
            in assertEqual "" expected actual
    ]

testAlphaEq = testGroup "Alpha equivalence" [
        testCase "Two free vars" $
            let a = TVar "a"
                b = TVar "b"
            in assertBool "" $ not $ a `alphaEq` b
      , testCase "Empty types" $
            let a = Forall "a" $ TVar "a"
                b = Forall "b" $ TVar "b"
            in assertBool "" $ a `alphaEq` b
      , testCase "Free and captured vars mixed" $
            let a = Forall "a" $ TVar "a"
                b = Forall "a" $ TVar "b"
            in assertBool "" $ not $ a `alphaEq` b
      , testCase "Names swap K-combinator" $
            let a = Forall "a" $ Forall "b" $ TVar "a" :-> TVar "b" :-> TVar "a"
                b = Forall "b" $ Forall "a" $ TVar "b" :-> TVar "a" :-> TVar "b"
            in assertBool "" $ a `alphaEq` b
      , testCase "Non-isomorphic types" $
            let a = Forall "a" $ Forall "b" $ TVar "a" :-> TVar "b"
                b = Forall "c" a
            in assertBool "" $ not $ a `alphaEq` b
    ]

testInferringVariableType = testGroup "Inferring variable type" [
        testCase "Variable is present in environment" $
            let env = Env [("y", TVar "b"), ("x", TVar "a"), ("z", TVar "c")]
                expr = Var "x"
                expected = pure $ TVar "a"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Variable is not present in environment" $
            let env = Env [("y", TVar "b"), ("z", TVar "c")]
                expr = Var "x"
                expected = throwError $ FreeVarNotTyped env "x"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Type variable shadowing" $
            let ty = Forall "c" $ TVar "a" :-> Forall "c" (TVar "c")
                env = Env [("x", ty)]
                expr = Var "x"
                expected = throwError $ FreeTVarShadowedInType env ("x", ty) "c"
                actual = inferType env expr
            in assertEqual "" expected actual
    ]

testInferringLambdaType = testGroup "Inferring lambda type" [
        testCase "Simple" $
            let env = Env [("y", TVar "b")]
                expr = Lam "x" (TVar "r") (Var "x")
                expected = pure $ TVar "r" :-> TVar "r"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Names clash" $
            let env = Env [("y", TVar "b"), ("x", TVar "a"), ("z", TVar "c")]
                expr = Lam "x" (TVar "r") (Var "x")
                expected = pure $ TVar "r" :-> TVar "r"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Type variable shadowing" $
            let ty = Forall "c" $ TVar "a" :-> Forall "c" (TVar "c")
                env = Env []
                expr = Lam "x" ty $ Var "x"
                expected = throwError $ FreeTVarShadowedInType env ("x", ty) "c"
                actual = inferType env expr
            in assertEqual "" expected actual
    ]

testInferringTypeLambdaType = testGroup "Inferring type lambda type" [
        testCase "Simple" $
            let env = Env []
                expr = TLam "r" $ Lam "x" (TVar "r") (Var "x")
                expected = pure $ Forall "r" $ TVar "r" :-> TVar "r"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Type variable shadowing" $
            let env = Env []
                subexpr = TLam "r" $ Lam "x" (TVar "r") (Var "x")
                expr = TLam "r" subexpr
                expected = throwError $ FreeTVarShadowed env subexpr "r"
                actual = inferType env expr
            in assertEqual "" expected actual
    ]

testInferringApplicationType = testGroup "Inferring application type" [
        testCase "Simple" $
            let env = Env [("f", TVar "a" :-> TVar "b"), ("x", TVar "a")]
                expr = Var "f" :@ Var "x"
                expected = pure $ TVar "b"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Partial application" $
            let env = Env [("f", TVar "a" :-> TVar "b" :-> TVar "c"), ("x", TVar "a")]
                expr = Var "f" :@ Var "x"
                expected = pure $ TVar "b" :-> TVar "c"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Left applicant is not an arrow" $
            let env = Env [("f", TVar "b"), ("x", TVar "a")]
                expr = Var "f" :@ Var "x"
                expected = throwError $ LeftApplicantIsNotArrow env (Var "f") (TVar "b")
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Argument type is quantified" $
            let env = Env [("f", Forall "a" (TVar "a") :-> TVar "b"), ("x", Forall "a" (TVar "a"))]
                expr = Var "f" :@ Var "x"
                expected = pure $ TVar "b"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Argument type is alpha-equivalent" $
            let env = Env [("f", Forall "b" (TVar "b") :-> TVar "c"), ("x", Forall "a" (TVar "a"))]
                expr = Var "f" :@ Var "x"
                expected = pure $ TVar "c"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Applicants types mismatch" $
            let env = Env [("f", TVar "b" :-> TVar "c"), ("x", TVar "a")]
                expr = Var "f" :@ Var "x"
                expected = throwError $ ApplicationTypesMismatch env
                    (Var "f", TVar "b" :-> TVar "c") (Var "x", TVar "a")
                actual = inferType env expr
            in assertEqual "" expected actual
    ]

testInferringTypeApplicationType = testGroup "Inferring type application type" [
        testCase "Basic" $
            let env = Env []
                expr = TLam "a" (Lam "x" (TVar "a") $ Var "x") :@* (TVar "b" :-> TVar "c")
                expected = pure $ (TVar "b" :-> TVar "c") :-> TVar "b" :-> TVar "c"
                actual = inferType env expr
            in assertEqual "" expected actual
    ]

testInferringTypeComplex = testGroup "Inferring type in complex cases" [
        testCase "K-combinator" $
            let env = Env []
                expr = Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x"))
                expected = pure $ TVar "a" :-> TVar "b" :-> TVar "a"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Lambda application" $
            let env = Env [("x", TVar "a"), ("y", TVar "b")]
                expr = Lam "x" (TVar "a") (Lam "y" (TVar "b") (Var "x")) :@ Var "x" :@ Var "y"
                expected = pure $ TVar "a"
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Lambda application but types mismatch" $
            let env = Env [("y", TVar "b")]
                lhs = Lam "x" (TVar "a") (Var "x")
                rhs = Var "y"
                expr = lhs :@ rhs
                expected = throwError $ ApplicationTypesMismatch env
                    (lhs, TVar "a" :-> TVar "a") (rhs, TVar "b")
                actual = inferType env expr
            in assertEqual "" expected actual

      , testCase "Free variable not typed" $
            let expr = Lam "x" (TVar "a") (Var "x" :@ Var "y")
                expected = throwError $ FreeVarNotTyped (Env [("x", TVar "a")]) "y"
                actual = inferType (Env []) expr
            in assertEqual "" expected actual

      , testCase "Type variable shadowing" $
            let env = Env []
                subexpr = TLam "c" $ Lam "x" (TVar "b") $ Lam "y" (TVar "c") $ Var "x"
                expr =  TLam "b" subexpr :@* Forall "a" (TVar "a" :-> TVar "c")
                expected = throwError $ FreeTVarShadowed env subexpr "c"
                actual = inferType env expr
            in assertEqual "" expected actual
    ]