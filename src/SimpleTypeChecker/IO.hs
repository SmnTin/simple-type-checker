{-# LANGUAGE FlexibleContexts #-}
module SimpleTypeChecker.IO ( parseExpression
                            , parseTypingRelation
                            , parseType
                            , parseEnvironment
                            ) where

import SimpleTypeChecker.Parser
import SimpleTypeChecker.Types
import SimpleTypeChecker.TypeChecker

import Control.Applicative (empty, Alternative (many, (<|>)))
import Data.Char (isLower, digitToInt, isDigit, isSpace, isUpper, isAlphaNum)
import Data.Foldable (Foldable(foldl'))
import Control.Monad.Except(MonadError(throwError, catchError))

lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

lowers :: Parser String
lowers = many lower

spaces :: Parser String
spaces = many $ satisfy isSpace

letter :: Parser Char
letter = lower <|> satisfy isUpper

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

string :: String -> Parser String
string = foldl' appendChar emptyParser
    where appendChar p c = (++) <$> p <*> (pure <$> char c)
          emptyParser = parser $ \s -> Just (s, "")


parseSymbol :: Parser Symb
parseSymbol = do
    h <- letter
    t <- many (alphaNum <|> char '_')
    q <- many (char '\'')
    _ <- spaces
    return (h : t ++ q)


parseExprVariable :: Parser Expr
parseExprVariable = Var <$> parseSymbol


parseTypeVariable :: Parser Type
parseTypeVariable = TVar <$> parseSymbol


parseForall :: Parser Type 
parseForall = do
    _    <- char '@' >> spaces
    symb <- parseSymbol
    _    <- char '.' >> spaces
    Forall symb <$> parseType


parseBrackets :: Char -> Char -> Parser a -> Parser a
parseBrackets left right m = do
    _ <- char left >> spaces
    x <- m
    _ <- char right >> spaces
    return x


parseExprBrackets :: Parser Expr
parseExprBrackets = parseBrackets '(' ')' parseExpression


parseTypeBrackets :: Parser Type
parseTypeBrackets = parseBrackets '(' ')' parseType


parseTypeAtom :: Parser Type
parseTypeAtom = parseTypeVariable <|>
                parseForall <|>
                parseTypeBrackets


parseType :: Parser Type
parseType = do
    _     <- spaces
    types <- many parseTypeArrow
    tail  <- parseTypeAtom
    _     <- spaces
    return $ foldr (:->) tail types

    where parseTypeArrow = do
            t <- parseTypeAtom
            _ <- spaces >> string "->" >> spaces
            return t


parseExpression :: Parser Expr
parseExpression = spaces >>
                  parseApplication <|>
                  parseAtom


parseAtom :: Parser Expr
parseAtom = parseLambda   <|>
            parseTypeLambda <|>
            parseExprVariable <|>
            parseExprBrackets


parseTypeApplicant :: Parser Type 
parseTypeApplicant = parseBrackets '[' ']' parseType

parseAtomOrTypeApplicant :: Parser (Either Expr Type)
parseAtomOrTypeApplicant = (Left <$> parseAtom) <|>
                           (Right <$> parseTypeApplicant)


parseApplication :: Parser Expr
parseApplication = do
    e  <- parseAtom
    es <- many parseAtomOrTypeApplicant
    return $ foldl' apply e es
        where apply e app = case app of
                Left atom -> e :@ atom
                Right ty  -> e :@* ty


parseLambdaVar :: Parser (Symb, Type)
parseLambdaVar = do
    symb <- parseSymbol
    _    <- char ':' >> spaces
    ty   <- parseType
    return (symb, ty)


parseLambda :: Parser Expr
parseLambda = do
    _          <- char '\\' >> spaces
    (symb, ty) <- parseLambdaVar
    _          <- char '.' >> spaces
    Lam symb ty <$> parseExpression


parseTypeLambda :: Parser Expr
parseTypeLambda = do
    _    <- char '#' >> spaces
    symb <- parseSymbol
    _    <- char '.' >> spaces
    TLam symb <$> parseExpression


parseDeclaration :: Parser (Symb, Type)
parseDeclaration = do
        symb <- parseSymbol
        _    <- char ':' >> spaces
        ty   <- parseType
        return (symb, ty)


parseNonEmptyEnvironment :: Parser Env
parseNonEmptyEnvironment = do
    _     <- spaces
    decls <- many parseWithComma
    tail  <- parseDeclaration
    return $ Env $ decls ++ [tail]

    where parseWithComma = do
            decl <- parseDeclaration
            _    <- char ',' >> spaces
            return decl


parseEnvironment :: Parser Env
parseEnvironment = parseNonEmptyEnvironment <|> (spaces >> return (Env []))


parseTypingRelation :: Parser TypingRelation
parseTypingRelation = do
    env   <- parseEnvironment
    _     <- string "|-" >> spaces
    expr  <- parseExpression
    _     <- char ':' >> spaces
    TypingRelation env expr <$> parseType


-- https://stackoverflow.com/questions/27471937/showsprec-and-operator-precedences

instance Show Type where
    showsPrec _ (TVar s)     = showString s

    showsPrec p (a :-> b)    = showParen (p > 3) $ 
        showsPrec 4 a . showString " -> " . showsPrec 3 b

    showsPrec p (Forall x t) = showParen (p > 0) $
        showChar '@' . showString x . showString ". " . shows t

showTypeAtom :: Type -> ShowS
showTypeAtom (TVar s) = showString s
showTypeAtom t        = showParen True $ shows t

instance Show Expr where
    showsPrec _ (Var s)     = showString s

    showsPrec p (a :@ b)    = showParen (p > 4) $ 
        showsPrec 4 a . showChar ' ' . showsPrec 5 b

    showsPrec p (a :@* b)   = showParen (p > 4) $ 
        showsPrec 4 a . showChar ' ' 
        . showChar '[' . shows b . showChar ']'

    showsPrec p (Lam s t e) = showParen (p > 0) $ 
        showChar '\\' . showString s . showString " : " 
        . showTypeAtom t . showString ". " . shows e

    showsPrec p (TLam s e) = showParen (p > 0) $ 
        showChar '#' . showString s . showString ". " . shows e


instance Show Env where
    showsPrec _ (Env [])      = showString ""

    showsPrec _ (Env (d:ds))  =
        foldl' appendDecl (showDecl d) ds
        where appendDecl sh d' =
                sh . showString ", " . showDecl d'
              showDecl (s, t) = 
                showString s . showString " : " . shows t

instance Show TypingRelation where
    showsPrec p (TypingRelation env expr t) = 
        showParen (p > 0) $ shows env . showVdash env 
        . shows expr . showString " : " . shows t
        where showVdash (Env e) = if null e
                                  then showString "|- "
                                  else showString " |- "

instance Show TypeError where

    show (FreeVarNotTyped env x) =
        "The type of the free variable " ++ show x
        ++ " is not given in the environment:\n"
        ++ "    " ++ show env

    show (LeftApplicantIsNotArrow env expr ty) =
        "The type of the left applicant is not an arrow type:\n"
        ++ "    " ++ show (TypingRelation env expr ty)

    show (ApplicationTypesMismatch env (e1, t1) (e2, t2)) =
        "The types of the applicants does not match:\n"
        ++ "    " ++ show (TypingRelation env e1 t1) ++ "\n"
        ++ "    " ++ show (TypingRelation env e2 t2)

    show (InferredAndGivenTypesMismatch rel inferred) =
        "The given typing relation is:\n"
        ++ "    " ++ show rel ++ "\n"
        ++ "but the inferred type is:\n"
        ++ "    " ++ show inferred

    show (FreeTVarShadowed env expr var) = 
        "The type variable " ++ show var ++ " is shadowed in the expression: \n"
        ++ "    " ++ show expr ++ "\n"
        ++ "in the environment:\n"
        ++ "    " ++ show env

    show (FreeTVarShadowedInType env (expr, ty) var) = 
        "The type variable " ++ show var ++ " is shadowed in the relation: \n"
        ++ "    " ++ show (TypingRelation env (Var expr) ty)

    show (TypeApplicationTypesMismatch env (expr, leftT) rightT) =
        "The type of the left applicant:\n"
        ++ "    " ++ show (TypingRelation env expr leftT) ++ "\n"
        ++ "does not match the right applicant:\n"
        ++ "    " ++ "[" ++ show rightT ++ "]"