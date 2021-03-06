module SimpleTypeChecker.TypeChecker( TypeError ( FreeVarNotTyped
                                                , LeftApplicantIsNotArrow
                                                , ApplicationTypesMismatch
                                                , InferredAndGivenTypesMismatch
                                                , FreeTVarShadowed
                                                , FreeTVarShadowedInType
                                                , TypeApplicationTypesMismatch
                                                )
                                    , extendEnv
                                    , alphaEq
                                    , inferType
                                    , checkTypingRelation 
                                    ) where

import SimpleTypeChecker.Types

import Control.Monad.Except (Except, MonadError (throwError), when, unless)
import Data.List (union, delete, (\\))


data TypeError = FreeVarNotTyped Env Symb
                    -- given environment and the free var
               | LeftApplicantIsNotArrow Env Expr Type
                    -- given environment, the left applicant and its inferred type
               | ApplicationTypesMismatch Env (Expr, Type) (Expr, Type)
                    -- given environment, the left and right applicants and their inferred types
               | InferredAndGivenTypesMismatch TypingRelation Type
                    -- the typing relation and its inferred type
               | FreeTVarShadowed Env Expr Symb
                    -- given environment, the invalid expression and the shadowed variable name
               | FreeTVarShadowedInType Env (Symb, Type) Symb
                    -- given environment, the free variable and its type and the shadowed variable name
               | TypeApplicationTypesMismatch Env (Expr, Type) Type
                    -- given environment, the left applicant and its inferred type and the right applicant
    deriving Eq


infixl 3 :->.

-- https://en.wikipedia.org/wiki/De_Bruijn_index
-- The same technique is used with types.
data DeBruijnType = CaptTVar Int
                  | FreeTVar Symb
                  | DeBruijnType :->. DeBruijnType
                  | Forall' DeBruijnType
    deriving Eq

toDeBruijn :: Type -> DeBruijnType
toDeBruijn = helper where
    helper :: Type -> DeBruijnType
    helper (TVar x)     = FreeTVar x
    helper (a :-> b)    = helper a :->. helper b
    helper (Forall x t) = Forall' $ captureFreeTVar x 0 $ helper t

    captureFreeTVar :: Symb -> Int -> DeBruijnType -> DeBruijnType
    captureFreeTVar _ _ (CaptTVar i)          = CaptTVar i
    captureFreeTVar v i (FreeTVar x) | x == v = CaptTVar i
    captureFreeTVar _ _ (FreeTVar x)          = FreeTVar x
    captureFreeTVar v i (a :->. b)            = captureFreeTVar v i a :->. captureFreeTVar v i b
    captureFreeTVar v i (Forall' t)           = Forall' $ captureFreeTVar v (i + 1) t


infix 1 `alphaEq`

alphaEq :: Type -> Type -> Bool 
alphaEq a b = toDeBruijn a == toDeBruijn b

-- Environment extension is just appending to the list
-- except the case when the given variable is already defined.
-- In this case we just override its type.
-- It is like the variable shadowing in some languages.
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env ((x, t) : e)) x' t' =
    if x' == x
    then Env $ (x, t') : e
    else case extendEnv (Env e) x' t' of
        Env e' -> Env $ (x, t) : e'
extendEnv (Env []) x' t' = Env [(x', t')]


type TVars = [Symb]

freeTVarsInType :: Type -> TVars
freeTVarsInType (TVar x)     = [x]
freeTVarsInType (a :-> b)    = freeTVarsInType a `union` freeTVarsInType b
freeTVarsInType (Forall x t) = delete x $ freeTVarsInType t

freeTVarsInExpr :: Expr -> TVars
freeTVarsInExpr (Var _)     = []
freeTVarsInExpr (a :@ b)    = freeTVarsInExpr a `union` freeTVarsInExpr b
freeTVarsInExpr (Lam _ t e) = freeTVarsInType t `union` freeTVarsInExpr e
freeTVarsInExpr (e :@* t)   = freeTVarsInExpr e `union` freeTVarsInType t
freeTVarsInExpr (TLam x e)  = delete x $ freeTVarsInExpr e


substTypeIntoType :: Symb -> Type -> Type -> Type 
substTypeIntoType x t (TVar y)     | y == x = t
substTypeIntoType x t (a :-> b)             = substTypeIntoType x t a :-> substTypeIntoType x t b
substTypeIntoType x t (Forall y o) | y /= x = Forall y $ substTypeIntoType x t o
substTypeIntoType _ _ ty                    = ty


checkShadowingInType :: Env -> Symb -> TVars -> Type -> Except TypeError ()
checkShadowingInType env freeVar freeTVars ty = helper freeTVars ty where 
    helper :: TVars -> Type -> Except TypeError ()
    helper freeTVars (TVar _) = return ()
    helper freeTVars (a :-> b) = do
        helper freeTVars a
        helper freeTVars b
    helper freeTVars (Forall x t) = do
        when (x `elem` freeTVars) $ throwError $ FreeTVarShadowedInType env (freeVar, ty) x 
        helper (x : freeTVars) t


inferType' :: Env -> TVars -> Expr -> Except TypeError Type

-- Type inference for a variable is just looking
-- for a type in the environment and checking if does not have
-- type variable shadowing.
inferType' env@(Env env') freeTVars (Var x) = case lookup x env' of
        Just ty -> do
            checkShadowingInType env x freeTVars ty
            return ty
        Nothing -> throwError $ FreeVarNotTyped env x

-- Type inference for an applition.
-- The left applicant should has an arrow type: a -> b
-- The type of the right should be alpha-equivalent to a.
--
-- Algorithm infers types of the both applicants and checks
-- that everything matches
inferType' env freeTVars (f :@ x) = do
    fType <- inferType' env freeTVars f
    xType <- inferType' env freeTVars x
    case fType of
        arg :-> res -> if arg `alphaEq` xType
                       then return res
                       else throwError $ ApplicationTypesMismatch env (f, fType) (x, xType)
        tvar        -> throwError $ LeftApplicantIsNotArrow env f tvar

-- Type inferring algorithm for a lambda abstraction 
-- just inferes the type of the lambda body and
-- then appends an arrow to the type.
--
-- For example:
-- if   |- M : b
-- then |- \x : a -> M  :  a -> b
inferType' env freeTVars (Lam arg argType expr) = do
    checkShadowingInType env arg freeTVars argType

    let extendedEnv = extendEnv env arg argType
    exprType <- inferType' extendedEnv freeTVars expr
    return $ argType :-> exprType

-- Type inferring algorithm for a type lambda abstraction 
-- just inferes the type of the lambda body and
-- then appends a quantifier to the type.
inferType' env freeTVars expr@(TLam var e) = do
    checkShadowing
    let freeTVars' = var : freeTVars
    Forall var <$> inferType' env freeTVars' e

    where checkShadowing :: Except TypeError ()
          checkShadowing = 
                when (var `elem` freeTVars) $ 
                    throwError $ FreeTVarShadowed env expr var

-- Type inference for a type applition.
-- The left applicant should has a quantifier type: @a. R
-- The type of the right is substituted into R instead of a.
inferType' env freeTVars (e :@* t) = do
    inferred <- inferType' env freeTVars e

    case inferred of
        Forall x t' -> return $ substTypeIntoType x t t'
        _           -> throwError $ TypeApplicationTypesMismatch env (e, inferred) t



inferType :: Env -> Expr -> Except TypeError Type
inferType env expr = inferType' env (freeTVarsInExpr expr) expr


checkTypingRelation :: TypingRelation -> Except TypeError ()
checkTypingRelation rel@(TypingRelation env expr givenType) = do
    inferredType <- inferType env expr

    if inferredType `alphaEq` givenType
    then return ()
    else throwError $ InferredAndGivenTypesMismatch rel inferredType
