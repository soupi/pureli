{-# LANGUAGE LambdaCase #-}

-- |execution module
module Main where

import System.Environment (getArgs)

import Run

main :: IO ()
main = getArgs >>= run
