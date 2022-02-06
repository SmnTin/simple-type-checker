module SimpleTypeChecker.TypeChecker where

import SimpleTypeChecker.Types

import Control.Monad.Except (Except, MonadError (throwError))


data TypeError = FreeVarNotTyped Env Symb
                    -- given environment and the free var
               | LeftApplicantIsNotArrow Env Expr Type
                    -- given environment, the left applicant and its inferred type
               | ApplicationTypesMismatch Env (Expr, Type) (Expr, Type)  
                    -- given environment, the left and right applicants and their inferred types
               | InferredAndGivenTypesMismatch TypingRelation Type                     
                    -- the inferred type of the typing relation 
    deriving Eq


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


inferType :: Env -> Expr -> Except TypeError Type

-- Type inference for a variable is just looking
-- for a type in the environment
inferType (Env env) (Var x) = case lookup x env of
    Just ty -> return ty
    Nothing -> throwError $ FreeVarNotTyped (Env env) x

-- Type inference for an applition is the most meaningful case.
-- The left applicant should has an arrow type: a -> b
-- The type of the right should then be: a
--
-- Algorithm infers types of the both applicants and checks
-- that everything matches
inferType env (f :@ x) = do
    fType <- inferType env f
    xType <- inferType env x
    case fType of
        arg :-> res -> if arg == xType 
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
inferType env (Lam arg argType expr) = do
    let extendedEnv = extendEnv env arg argType
    exprType <- inferType extendedEnv expr
    return $ argType :-> exprType


checkTypingRelation :: TypingRelation -> Except TypeError ()
checkTypingRelation rel@(TypingRelation env expr givenType) = do
    inferredType <- inferType env expr
    if inferredType == givenType
    then return ()
    else throwError $ InferredAndGivenTypesMismatch rel inferredType
