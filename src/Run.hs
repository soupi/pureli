{-
 ===============
    NOT READY
 ===============
 -}

{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Run where

import Control.Monad (unless)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.IO (hFlush, stdout)

import AST
import Utils
import Parser
import Module
import Eval

-- |
-- interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []     -> putStrLn welcome_message >> repl >> putStrLn (unlines ["","Goodbye."]) -- |^run repl
    [file] -> runExceptT (loadModule file "main" >>= evalModule) >>= \case -- |^interpret file
      Left err                -> print err
      Right (WithMD _ result) -> print result
    _      -> putStrLn "Error. too many arguments"


-- |
-- tries to parse the expression and interpret it.
runExpr :: String -> IO (Either Error (WithMD Expr))
runExpr content =
  case getValFromExpr "REPL" content of
    Right expr -> runExceptT $ runReaderT (eval expr) initEvalState
    Left err   -> runExceptT $ throwE (Error Nothing err)

-- |
-- a REPL for expressions.
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  expr <- getLine
  unless (expr == ":q") $ do
    case getValFromExpr "REPL" expr of
      Left  err -> putStrLn err
      Right res ->  runExceptT (runReaderT (eval res) initEvalState) >>= \case
        Left err     -> print err
        Right result -> print result
    repl

-- |
-- a welcome message to be shown when starting the REPL
welcome_message :: String
welcome_message = unlines ["REPL for lang, a purely functional, dynamically typed,"
                          ,"Lisp-like programming language version 0.0.1"
                          ,"Write an expression and press enter, or :q to Quit"]
