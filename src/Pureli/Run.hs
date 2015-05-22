
{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Pureli.Run (run) where

import Control.Monad (unless)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Directory (setCurrentDirectory)
import qualified System.FilePath.Posix   as P (takeDirectory, takeFileName)
import qualified System.FilePath.Windows as W (takeDirectory, takeFileName)

import Pureli.AST
import Pureli.Parser
import Pureli.Module
import Pureli.Eval

-- |
-- interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []     -> putStrLn welcome_message >> repl >> putStrLn (unlines ["","Goodbye."]) -- |^run repl
    [file] ->
      let (takeDirectory, takeFileName) = systemFuncs
      in setCurrentDirectory (takeDirectory file) >> runExceptT (loadModule (takeFileName file) "main" >>= evalModule) >>= \case -- |^interpret file
        Left err                -> print err
        Right (WithMD _ result) -> print result
    _      -> putStrLn "Error. too many arguments"


-- |
-- get relative system functions (windows or posix)
systemFuncs :: (FilePath -> FilePath, FilePath -> FilePath)
systemFuncs =
  if os == "mingw32"
  then (W.takeDirectory,W.takeFileName)
  else (P.takeDirectory,P.takeFileName)

-- |
-- tries to parse the expression and interpret it.
runExpr :: String -> IO ()
runExpr content =
    case parseExpr "REPL" content of
      Left  err -> putStrLn err
      Right res ->  runExceptT (runReaderT (eval res) initEvalState) >>= \case
        Left err     -> print err
        Right result -> print result

-- |
-- a REPL for expressions.
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  expr <- getLine
  unless (expr == ":q") $ do
    runExpr expr
    repl

-- |
-- a welcome message to be shown when starting the REPL
welcome_message :: String
welcome_message = unlines ["REPL for lang, a purely functional, dynamically typed,"
                          ,"Lisp-like programming language version 0.0.1"
                          ,"Write an expression and press enter, or :q to Quit"]
