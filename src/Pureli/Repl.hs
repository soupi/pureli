
{-# LANGUAGE LambdaCase #-}

-- |
-- repl module
module Pureli.Repl (runRepl) where

import Control.Monad (unless)
import System.IO (hFlush, stdout)

--import Pureli.Utils
import Pureli.AST
import Pureli.Parser
import Pureli.Module
import Pureli.Eval

-- |
-- interprets a file or runs the REPL depending on the number of arguments
runRepl :: IO ()
runRepl =
  putStrLn welcomeMsg >> repl replModule >> putStrLn (unlines ["","Goodbye."]) -- |^run repl

-- |
-- tries to parse the expression and interpret it.
runExpr :: String -> Module -> IO Module
runExpr content modul =
    case parseReqDefExp "REPL" content of
      Left  err -> putStrLn err >> return modul
      Right (Exp res) ->
        (evalExpr modul res >>= either print print) >> return modul
      Right (Req res) ->
        requireToMod res >>= either (\err -> print err >> return modul) (return . flip addImport modul)
      Right (Def res) -> return $ addToEnv res modul


-- |
-- a REPL for expressions.
repl :: Module -> IO ()
repl modul = do
  putStr "> "
  hFlush stdout
  expr <- getLine
  unless (expr == ":q") $ repl =<< runExpr expr modul

-- |
-- a welcome message to be shown when starting the REPL
welcomeMsg :: String
welcomeMsg = unlines ["REPL for lang, a purely functional, dynamically typed,"
                     ,"Lisp-like programming language version 0.0.1"
                     ,"Write an expression and press enter, or :q to Quit"]
