{-# LANGUAGE FlexibleContexts #-}
module SimpleTypeChecker.IO where

import SimpleTypeChecker.Parser
import SimpleTypeChecker.Types

import Control.Applicative (empty, Alternative (many, (<|>)))
import Data.Char (isLower, digitToInt, isDigit, isSpace, isUpper, isAlphaNum)
import Data.Foldable (Foldable(foldl'))
import Control.Monad.Except(MonadError(throwError, catchError))

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = parser f where
  f (c:cs) | pred c  = Just (cs,c)
  f _                = Nothing

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
string (x:xs) = foldl' (\p c -> (++) <$> p <*> (pure <$> char c)) (pure <$> char x) xs
string []     = undefined


parseSymbol :: Parser Symb
parseSymbol = do
    h <- letter
    t <- many alphaNum
    _ <- spaces
    return (h:t)


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
    types <- many parseTypeArrow
    tail  <- parseTypeAtom
    _     <- spaces
    return $ foldr (:->) tail types

    where parseTypeArrow = do
            t <- parseTypeAtom
            _ <- spaces >> string "->" >> spaces
            return t


parseExpression :: Parser Expr
parseExpression = parseApplication <|>
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
    showsPrec p (a :@ b)    = showParen (p > 4) $ showsPrec 4 a . showChar ' ' . showsPrec 5 b
    showsPrec p (Lam s t e) = showParen (p > 0) $ showChar '\\' . showString s . showString " : " . showTypeAtom t . showString " -> " . shows e

instance Show Env where
    showsPrec _ (Env [])      = showString ""
    showsPrec _ (Env (d:ds))  = foldl' (\sh d' -> sh . showString ", " . showDecl d') (showDecl d) ds 
        where showDecl (s, t) = showString s . showString " : " . shows t

instance Show TypingRelation where
    showsPrec _ (TypingRelation env expr t) = shows env . showString " |- " . shows expr . showString " : " . shows t