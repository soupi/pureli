{-# LANGUAGE LambdaCase #-}

-- |execution module
module Main where

import System.Environment (getArgs)

import Pureli.Run

main :: IO ()
main = getArgs >>= run
