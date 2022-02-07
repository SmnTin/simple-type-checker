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
string = foldl' (\p c -> (++) <$> p <*> (pure <$> char c)) emptyParser
    where emptyParser = parser $ \s -> Just (s, "")


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


parseBrackets :: Parser a -> Parser a
parseBrackets m = do
    _ <- char '(' >> spaces
    x <- m
    _ <- char ')' >> spaces
    return x


parseExprBrackets :: Parser Expr
parseExprBrackets = parseBrackets parseExpression


parseTypeBrackets :: Parser Type
parseTypeBrackets = parseBrackets parseType


parseTypeAtom :: Parser Type
parseTypeAtom = parseTypeVariable <|>
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
            parseExprVariable <|>
            parseExprBrackets


parseApplication :: Parser Expr
parseApplication = do
    (e : es) <- many parseAtom
    return $ foldl' (:@) e es


parseAbstractedVar :: Parser (Symb, Type)
parseAbstractedVar = do
    symb <- parseSymbol
    _    <- char ':' >> spaces
    ty   <- parseTypeAtom
    return (symb, ty)


parseLambda :: Parser Expr
parseLambda = do
    _          <- char '\\' >> spaces
    (symb, ty) <- parseAbstractedVar
    _          <- string "->" >> spaces
    Lam symb ty <$> parseExpression


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
    showsPrec _ (TVar s)  = showString s
    showsPrec p (a :-> b) = showParen (p > 3) $ showsPrec 4 a . showString " -> " . showsPrec 3 b

showTypeAtom :: Type -> ShowS
showTypeAtom (TVar s) = showString s
showTypeAtom t        = showParen True $ shows t

instance Show Expr where
    showsPrec _ (Var s)     = showString s

    showsPrec p (a :@ b)    = showParen (p > 4) $ 
        showsPrec 4 a . showChar ' ' . showsPrec 5 b

    showsPrec p (Lam s t e) = showParen (p > 0) $ 
        showChar '\\' . showString s . showString " : " 
        . showTypeAtom t . showString " -> " . shows e

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