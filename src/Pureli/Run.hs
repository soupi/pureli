
{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Pureli.Run (run) where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import System.Directory (setCurrentDirectory, getCurrentDirectory)

import Pureli.Utils (systemFuncs, displayCommands, dashOpener, withCyan)
import Pureli.AST
import Pureli.Module
import Pureli.Eval
import Pureli.Repl

-- |
-- Interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []          -> runRepl -- run repl
    ["--help"]  -> putStrLn helpMsg -- print help message
    ["--version"] -> putStrLn versionMsg
    ("--test":file:argv) -> do
      hSetBuffering stdout NoBuffering
      runModule "test" file argv
    (file:argv) -> do
      hSetBuffering stdout NoBuffering
      runModule "main" file argv

-- |
-- Interprets a file and run a selected module in it
runModule :: Name -> FilePath -> [String] -> IO ()
runModule mName file argv = do
  let args = WithMD emptyMeta $ QUOTE $ WithMD emptyMeta $ LIST $ fmap (WithMD emptyMeta . ATOM . String) argv
  let (takeDirectory, takeFileName) = systemFuncs
  currDir <- getCurrentDirectory
  setCurrentDirectory (takeDirectory file)
  result <- runExceptT $ do
    modul <- loadModule (takeFileName file) mName
    lift $ setCurrentDirectory currDir
    evalModule (addToEnv ("argv", args) modul)
  case result of
    Left err                -> print err
    Right _ -> return ()

-- |
-- A help message to display users
helpMsg :: String
helpMsg = unlines (msg ++ displayCommands dashOpener usageMsg)
  where msg =
          [""
          ,versionMsg
          ,""
          ,"Usage:"
          ]

-- |
-- A version message to display users
versionMsg :: String
versionMsg = withCyan "Pureli " ++ "interpreter version 0.6.0"

-- |
-- A usage message to display users, describing the possible operations of pureli
usageMsg :: [(String, String)]
usageMsg =
    [("pureli", "Runs the read-eval-print loop")
    ,("pureli <file>", "Interprets and runs a source file with main/main as entry point")
    ,("pureli --test <file>", "Interprets and runs a source file with test/main as entry point")
    ,("pureli --version", "Shows the version of the interpreter")
    ,("pureli --help", "Shows this help message")
    ]

