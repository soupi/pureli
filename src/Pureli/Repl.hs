
{-# LANGUAGE LambdaCase #-}

-- |
-- repl module
module Pureli.Repl (runRepl) where

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
runExpr :: Module -> String -> IO Module
runExpr modul ""      = return modul
runExpr modul content =
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
  case expr of
    ":reset" -> repl replModule
    ":start" -> repl =<< runExpr modul =<< multiLineExpr
    ":help"  -> putStrLn helpMsg >> repl modul
    ":q"     -> return ()
    _        -> repl =<< runExpr modul expr


-- |
-- read a multiple line expression until option ':end' or ':trash'
multiLineExpr :: IO String
multiLineExpr = go []
  where go exps =
          getLine >>= \case
            ":end"   -> return $ concat $ reverse exps
            ":trash" -> return []
            expr     -> go (expr:exps)


-- |
-- a welcome message to be shown when starting the REPL
welcomeMsg :: String
welcomeMsg = unlines ["REPL for lang, a purely functional, dynamically typed,"
                     ,"Lisp-like programming language version 0.0.1"
                     ,"Write an expression and press enter, :help or help or :q to quit"]

-- |
-- a help message to be shown when user enters :help
helpMsg :: String
helpMsg = unlines $ (["","  Available Commands:",""] ++) $ displayCommands commandsDesc

-- |
-- a list of available commands and their description
commandsDesc :: [(String, String)]
commandsDesc =
  [(":help",  "Displays this help guide")
  ,(":start", "Will read multiple lines expression until either :end or :trash")
  ,(":end",   "Comes after :start and will evaluate the multiline expression")
  ,(":trash", "Comes after :start and will throw away the multiline expression without evaluating")
  ,(":reset", "Reset environment")
  ,(":q",     "Quit repl")]

displayCommands :: [(String, String)] -> [String]
displayCommands cmds = fmap (displayCommand (cmdWithSpaces n)) cmds
  where n = 8 + foldr (\str acc -> max (length (fst str)) acc) 0 cmds

cmdWithSpaces :: Int -> String -> String
cmdWithSpaces n str = str ++ replicate (n - length str) ' '

displayCommand :: (String -> String) -> (String, String) -> String
displayCommand paddCmd (cmd, desc) = "    " ++ paddCmd cmd ++ desc
