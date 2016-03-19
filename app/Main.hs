{-# LANGUAGE LambdaCase #-}

-- |execution module
module Main where

import System.Environment (getArgs)

import Language.Pureli.Run

main :: IO ()
main = getArgs >>= run
