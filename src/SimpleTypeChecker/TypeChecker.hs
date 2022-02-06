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


extendEnv :: Env -> Symb -> Type -> Env 
extendEnv (Env ((x, t) : e)) x' t' = 
    if x' == x 
    then Env $ (x, t') : e
    else case extendEnv (Env e) x' t' of
        Env e' -> Env $ (x, t) : e'
extendEnv (Env []) x' t' = Env [(x', t')]


inferType :: Env -> Expr -> Except TypeError Type

inferType (Env env) (Var x) = case lookup x env of
    Just ty -> return ty
    Nothing -> throwError $ FreeVarNotTyped (Env env) x

inferType env (f :@ x) = do
    fType <- inferType env f
    xType <- inferType env x
    case fType of
        arg :-> res -> if arg == xType 
                       then return res
                       else throwError $ ApplicationTypesMismatch env (f, fType) (x, xType)
        tvar        -> throwError $ LeftApplicantIsNotArrow env f tvar

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
