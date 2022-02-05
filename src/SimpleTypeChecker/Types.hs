module SimpleTypeChecker.Types where

infixl 4 :@
infixr 3 :->

-- Symbol. It is a name token of expression and type variables
type Symb = String

-- Expression. Uses Church style, i.e. abstracted variables are annotated with types
data Expr = Var Symb             -- An expression variable
          | Expr :@ Expr         -- An application
          | Lam Symb Type Expr   -- A lambda abstraction
    deriving Eq

data Type = TVar Symb            -- A type variable
          | Type :-> Type        -- An arrow (a mapping from one type to another)
    deriving Eq

-- Environment. It is a list of declarations
newtype Env = Env [(Symb,Type)]
    deriving Eq


data TypingRelation = TypingRelation Env Expr Type
    deriving Eq
