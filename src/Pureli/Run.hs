
{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Pureli.Run (run) where

import Control.Monad.Trans.Except
import System.Directory (setCurrentDirectory)

import Pureli.Utils (systemFuncs, displayCommands, dashOpener)
import Pureli.AST
import Pureli.Module
import Pureli.Eval
import Pureli.Repl

-- |
-- interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []         -> runRepl -- |^run repl
    ["--help"] -> putStrLn helpMsg -- |^print help message
    [file]     ->
      let (takeDirectory, takeFileName) = systemFuncs
      in setCurrentDirectory (takeDirectory file) >> runExceptT (loadModule (takeFileName file) "main" >>= evalModule) >>= \case -- |^interpret file
        Left err                -> print err
        Right (WithMD _ result) -> print result
    _      -> putStrLn "Error. too many arguments"


helpMsg :: String
helpMsg = unlines (msg ++ displayCommands dashOpener usageMsg)
  where msg =
          [""
          ,"Pureli interpreter version 0.2.0"
          ,""
          ,"Usage:"
          ]

usageMsg :: [(String, String)]
usageMsg =
    [("pureli", "Runs the read-eval-print loop")
    ,("pureli <file>", "Interprets and runs a source file")
    ]

