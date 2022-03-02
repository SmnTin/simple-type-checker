module SimpleTypeChecker.Types.TestBindings where

import SimpleTypeChecker.Types


boolT :: Type
boolT = Forall "a" $ TVar "a" :-> TVar "a" :-> TVar "a"  -- @a. a -> a -> a

tru :: Expr
tru = TLam "a" $ Lam "t" (TVar "a") $ Lam "f" (TVar "a") $ Var "t"  -- #a. \t : a. \f : a. t

fls :: Expr
fls = TLam "a" $ Lam "t" (TVar "a") $ Lam "f" (TVar "a") $ Var "f"  -- #a. \t : a. \f : a. f


emptyT :: Type
emptyT = Forall "a" $ TVar "a" -- @a. a

unitT :: Type 
unitT = Forall "a" (TVar "a" :-> TVar "a") -- @a. a -> a


id' :: Expr 
id' = TLam "a" $ Lam "x" (TVar "a") $ Var "x" -- #a. \x : a. x

idT :: Type 
idT = unitT


kComb :: Expr
kComb = TLam "a" $ TLam "b" $ Lam "x" (TVar "a") $ Lam "y" (TVar "b") $ Var "x"  -- #a. #b. \x : a. \y : b. x

kCombT :: Type 
kCombT = Forall "a" $ Forall "b" $ TVar "a" :-> TVar "b" :-> TVar "a"  -- @a. @b. a -> b -> a