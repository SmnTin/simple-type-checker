{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleTypeChecker.Parser ( Parser
                                , parser
                                , runParser
                                , runParserFully
                                , parserToReadsPrec
                                , satisfy
                                ) where

import Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import Control.Applicative (Alternative ((<|>)), empty)
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

-- Monadic parser that is used to parse lambda expressions from the command prompt
newtype Parser a =
    Parser { runParser :: String -> Maybe (String, a) }

parser :: (String -> Maybe (String, a)) -> Parser a
parser = Parser

runParserFully :: Parser a -> String -> Maybe a
runParserFully (Parser f) s = case f s of
    Just ([], r) -> Just r
    _            -> Nothing 

parserToReadsPrec :: Parser a -> Int -> ReadS a
parserToReadsPrec p _ s = [swap $ fromJust $ runParser p s]


instance Functor Parser where
    fmap :: forall a b. (a -> b) -> Parser a -> Parser b
    fmap = coerce ( fmap . fmap . fmap
        :: (a -> b) -> (String -> Maybe (String, a)) -> (String -> Maybe (String, b)) )

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> pure (s, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser u <*> Parser v = Parser f where
        f xs = do
            (xs', fun) <- u xs
            (xs'', arg) <- v xs'
            return (xs'', fun arg)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const empty

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser u <|> Parser v = Parser f where
        f xs = u xs `catchError` const (v xs)

instance Monad Parser where
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser u) >>= f = Parser g where
        g xs = do
            (xs', y) <- u xs
            runParser (f y) xs'

instance MonadFail Parser where
    fail :: String -> Parser a
    fail _ = empty


satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = parser f where
  f (c:cs) | pred c  = Just (cs,c)
  f _                = Nothing