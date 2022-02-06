module SimpleTypeChecker.AppMain where

import SimpleTypeChecker.Parser (runParserFully)
import SimpleTypeChecker.IO (parseTypingRelation)
import SimpleTypeChecker.TypeChecker (checkTypingRelation, TypeError)
import SimpleTypeChecker.Types (TypingRelation)

import System.Environment (getArgs)
import GHC.Base (failIO)
import Control.Monad.Except (Except, MonadError (throwError), runExcept, withExcept)


data MainError = ExtraArguments Int
               | ParsingError
               | TypingError TypeError

instance Show MainError where
    show (ExtraArguments num) = "Expected exactly one argument but given " ++ show num
    show ParsingError         = "Given typing relation is not syntatically correct"
    show (TypingError err)    = show err


validateArgs :: [String] -> Except MainError ()
validateArgs [_] = return ()
validateArgs args = throwError $ ExtraArguments $ length args

parseExcept :: String -> Except MainError TypingRelation
parseExcept arg = case runParserFully parseTypingRelation arg of
    Just rel -> return rel
    Nothing  -> throwError ParsingError

mainExcept :: [String] -> Except MainError ()
mainExcept args = do
    validateArgs args
    let [arg] = args

    rel <- parseExcept arg
    TypingError `withExcept` checkTypingRelation rel


main :: IO ()
main = do
    args <- getArgs
    case runExcept $ mainExcept args of
        Right () -> putStrLn "Everything is okay :)"
        Left err -> print err

    return ()