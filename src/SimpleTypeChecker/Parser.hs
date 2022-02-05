{-# LANGUAGE InstanceSigs #-}
module SimpleTypeChecker.Parser (Parser) where

import Control.Monad.Except (Except, MonadError (catchError))
import Control.Applicative (Alternative ((<|>)), empty)
import Data.Coerce (coerce)

-- Monadic parser that is used to parse lambda expressions from the command prompt
newtype Parser e a =
    Parser { runParser :: String -> Except e (String,a) }

instance Functor (Parser e) where
    fmap :: (a -> b) -> Parser e a -> Parser e b
    fmap = coerce ( fmap . fmap . fmap
        :: (a -> b) -> (String -> Except e (String, a)) -> String -> Except e (String, b) )

instance Applicative (Parser e) where
    pure :: a -> Parser e a
    pure x = Parser $ \s -> pure (s, x)

    (<*>) :: Parser e (a -> b) -> Parser e a -> Parser e b
    Parser u <*> Parser v = Parser f where
        f xs = do
            (xs', fun) <- u xs
            (xs'', arg) <- v xs'
            return (xs'', fun arg)

instance Monoid e => Alternative (Parser e) where
    empty :: Parser e a
    empty = Parser $ const empty

    (<|>) :: Parser e a -> Parser e a -> Parser e a
    Parser u <|> Parser v = Parser f where
        f xs = u xs `catchError` const (v xs)

instance Monad (Parser e) where
    return = pure

    (>>=) :: Parser tok a -> (a -> Parser tok b) -> Parser tok b
    (Parser u) >>= f = Parser g where 
        g xs = do
            (xs', y) <- u xs
            runParser (f y) xs'
            
instance Monoid e => MonadFail (Parser e) where
    fail :: String -> Parser e a
    fail _ = empty
