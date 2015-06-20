
{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Pureli.Run (run) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import System.Directory (setCurrentDirectory, getCurrentDirectory)

import Pureli.Utils (systemFuncs, displayCommands, dashOpener)
import Pureli.AST
import Pureli.Module
import Pureli.Eval
import Pureli.Repl

-- |
-- interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []          -> runRepl -- |^run repl
    ["--help"]  -> putStrLn helpMsg -- |^print help message
    (file:argv) -> do
      let args = WithMD emptyMeta $ QUOTE $ WithMD emptyMeta $ LIST $ fmap (WithMD emptyMeta . ATOM . String) argv
      let (takeDirectory, takeFileName) = systemFuncs
      currDir <- getCurrentDirectory
      setCurrentDirectory (takeDirectory file)
      result <- runExceptT $ do
        modul <- loadModule (takeFileName file) "main"
        lift $ setCurrentDirectory currDir
        evalModule (addToEnv ("argv", args) modul)
      case result of
        Left err                -> print err
        Right _ -> return ()


helpMsg :: String
helpMsg = unlines (msg ++ displayCommands dashOpener usageMsg)
  where msg =
          [""
          ,"Pureli interpreter version 0.4.0"
          ,""
          ,"Usage:"
          ]

usageMsg :: [(String, String)]
usageMsg =
    [("pureli", "Runs the read-eval-print loop")
    ,("pureli <file>", "Interprets and runs a source file")
    ]

