
-- |
-- how to parse a number
module Pureli.ParseNumber (parseNumber) where

import Text.ParserCombinators.Parsec hiding (spaces)

import Pureli.AST

parseNumber :: Parser Atom
parseNumber = try parseNum

parseNum :: Parser Atom
parseNum = do
    s <- optionMaybe $ char '-'
    whole <- many1 digit
    c <- optionMaybe ( char '.')
    case c of
        Nothing -> (return . Integer . read) (sign s ++ whole)
        Just _  -> do
            part <- many1 digit
            (return . Real . read) (sign s ++ whole ++ "." ++ part)

sign :: Maybe a -> String
sign s = case s of { Nothing -> ""; Just _ -> "-" }

