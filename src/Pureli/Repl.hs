
{-# LANGUAGE LambdaCase #-}

-- |
-- read-evaluate-print-loop module
module Pureli.Repl (runRepl) where

import qualified System.Console.Haskeline as HL
import Control.Monad.Trans.Class (lift)

--import Pureli.Utils
import Pureli.AST
import Pureli.Utils
import Pureli.Parser
import Pureli.Module
import Pureli.Eval

-- |
-- interprets a file or runs the REPL depending on the number of arguments
runRepl :: IO ()
runRepl = do
  putStrLn welcomeMsg
  HL.runInputT HL.defaultSettings (repl replModule) -- |^run repl
  putStrLn (unlines ["","Goodbye."])

-- |
-- tries to parse the expression and interpret it.
runExpr :: Module -> String -> HL.InputT IO Module
runExpr modul ""      = return modul
runExpr modul content =
    case parseReqDefExp "REPL" content of
      Left  err -> HL.outputStrLn err >> return modul
      Right (Exp res) ->
        lift $ (evalExpr modul ("", res) >>= either print print) >> return modul
      Right (Req res) ->
        lift (requireToMod res) >>= either (\err -> HL.outputStrLn (show err) >> return modul) (return . flip addImport modul)
      Right (Def res) ->
        lift (evalExpr modul res) >>= \case
          Left  err  -> HL.outputStrLn (show err) >> return modul
          Right expr -> return $ addToEnv (fst res, expr) modul


-- |
-- a REPL for expressions.
repl :: Module -> HL.InputT IO ()
repl modul = do
  minput <- HL.getInputLine "> "
  case minput of
    Just ",reset" -> repl replModule
    Just ",start" -> repl =<< runExpr modul =<< multiLineExpr
    Just ",help"  -> HL.outputStrLn helpMsg >> repl modul
    Just ",env"   -> HL.outputStrLn (unlines $ listMods 0 modul) >> repl modul
    Just ",q"     -> return ()
    Just c@(',':_)-> HL.outputStrLn ("*** Error: " ++ c ++ " unknown command.") >> repl modul
    Just expr     -> repl =<< runExpr modul expr
    Nothing       -> return ()

-- |
-- read a multiple line expression until option ':end' or ':trash'
multiLineExpr :: HL.InputT IO String
multiLineExpr = go []
  where go exps =
          HL.getInputLine "" >>= \case
            Nothing       -> go exps
            Just ":end"   -> return $ concat $ reverse exps
            Just ":trash" -> return []
            Just expr     -> go (expr:exps)


-- |
-- a welcome message to be shown when starting the REPL
welcomeMsg :: String
welcomeMsg = unlines ["REPL for Pureli, a purely functional, dynamically typed,"
                     ,"Lisp-like programming language version 0.4.0"
                     ,"Write an expression and press enter to evaluate, :help for help or :q to quit"]

-- |
-- a help message to be shown when user enters :help
helpMsg :: String
helpMsg = unlines $ (["","  Available Commands:",""] ++) $ displayCommands spaceOpener commandsDesc

-- |
-- a list of available commands and their description
commandsDesc :: [(String, String)]
commandsDesc =
  [(",help",  "Displays this help guide")
  ,(",start", "Will read multiple lines expression until either :end or :trash")
  ,(",end",   "Comes after :start and will evaluate the multiline expression")
  ,(",trash", "Comes after :start and will throw away the multiline expression without evaluating")
  ,(",reset", "Reset environment")
  ,(",q",     "Quit repl")]

