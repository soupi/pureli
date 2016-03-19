
{-# LANGUAGE LambdaCase #-}

-- |
-- read-evaluate-print-loop module
module Language.Pureli.Repl (runRepl) where

import qualified System.Console.Haskeline as HL
import qualified System.Console.ANSI as ANSI
import Control.Monad.Trans.Class (lift)

--import Pureli.Utils
import Language.Pureli.AST
import Language.Pureli.Utils
import Language.Pureli.Parser
import Language.Pureli.Module
import Language.Pureli.Eval
import Language.Pureli.Printer

-- |
-- interprets a file or runs the REPL depending on the number of arguments
runRepl :: IO ()
runRepl = do
  putStrLn welcomeMsg
  HL.runInputT HL.defaultSettings (repl replModule) -- run repl
  putStrLn (unlines ["","Goodbye."])

-- |
-- tries to parse the expression and interpret it.
runExpr :: Module -> String -> HL.InputT IO Module
runExpr modul ""      = pure modul
runExpr modul content =
    case parseReqDefExp "REPL" content of
      Left  err -> HL.outputStrLn err >> pure modul
      Right (Exp res) ->
        lift $ (evalExpr modul ("", res) >>= either print printExpr) >> pure modul
      Right (Req res) ->
        lift (requireToMod res) >>= either (\err -> HL.outputStrLn (show err) >> pure modul) (pure . flip addImport modul)
      Right (Def res) ->
        lift (evalExpr modul res) >>= \case
          Left  err  -> HL.outputStrLn (show err) >> pure modul
          Right expr -> pure $ addToEnv (fst res, expr) modul

printExpr :: WithMD Expr -> IO ()
printExpr (WithMD _ (QUOTE (WithMD _ (IOResult (WithMD _ (ATOM Nil)))))) = pure ()
printExpr (WithMD _ (QUOTE (WithMD _ (IOResult result)))) = putStrLn $ printer result
printExpr val = putStrLn $ printer val


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
    Just ",clear" -> lift ANSI.clearScreen  >> repl modul
    Just ",q"     -> pure ()
    Just c@(',':_)-> HL.outputStrLn (withRed "*** Error: " ++ withYellow c ++ " unknown command.") >> repl modul
    Just expr     -> repl =<< runExpr modul expr
    Nothing       -> pure ()

-- |
-- read a multiple line expression until option ':end' or ':trash'
multiLineExpr :: HL.InputT IO String
multiLineExpr = go []
  where go exps =
          HL.getInputLine "" >>= \case
            Nothing       -> go exps
            Just ",end"   -> pure $ concat $ reverse exps
            Just ",trash" -> pure []
            Just expr     -> go (expr:exps)


-- |
-- a welcome message to be shown when starting the REPL
welcomeMsg :: String
welcomeMsg = unlines ["REPL for " ++ withCyan "Pureli" ++ ", a purely functional, dynamically typed,"
                     ,"Lisp-like programming language version 0.6.0"
                     ,"Write an expression and press enter to evaluate, " ++ withGreen ",help" ++ " for help or " ++ withGreen ",q" ++ " to quit"]

-- |
-- a help message to be shown when user enters :help
helpMsg :: String
helpMsg = unlines $ (["","  Available Commands:",""] ++) $ displayCommands spaceOpener commandsDesc

-- |
-- a list of available commands and their description
commandsDesc :: [(String, String)]
commandsDesc =
  [(",help",  "Displays this help guide")
  ,(",start", "Will read multiple lines expression until either ,end or ,trash")
  ,(",end",   "Comes after ,start and will evaluate the multiline expression")
  ,(",trash", "Comes after ,start and will throw away the multiline expression without evaluating")
  ,(",reset", "Reset environment")
  ,(",clear", "Clear the screen")
  ,(",q",     "Quit repl")]

