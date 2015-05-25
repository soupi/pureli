
{-# LANGUAGE LambdaCase #-}

-- |
-- execution module
module Pureli.Run (run) where

import Control.Monad.Trans.Except
import System.Directory (setCurrentDirectory)

import Pureli.Utils (systemFuncs)
import Pureli.AST
import Pureli.Module
import Pureli.Eval
import Pureli.Repl

-- |
-- interprets a file or runs the REPL depending on the number of arguments
run :: [String] -> IO ()
run = \case
    []     -> runRepl -- |^run repl
    [file] ->
      let (takeDirectory, takeFileName) = systemFuncs
      in setCurrentDirectory (takeDirectory file) >> runExceptT (loadModule (takeFileName file) "main" >>= evalModule) >>= \case -- |^interpret file
        Left err                -> print err
        Right (WithMD _ result) -> print result
    _      -> putStrLn "Error. too many arguments"

