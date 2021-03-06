
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- A parser for the language
module Language.Pureli.Parser (parseExpr, parseFile, parseReqDefExp, getMDSource) where

import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Data.Traversable (forM)
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P (Parser)

import Language.Pureli.Utils (duplicates)
import Language.Pureli.AST
import Language.Pureli.Printer()
import qualified Language.Pureli.Lexer as L

parensOrBrackets :: P.Parser a -> P.Parser a
parensOrBrackets parser = L.parens parser <|> L.brackets parser


-----------
-- Atoms
-----------

atom :: P.Parser Atom
atom =  parseNumber
    <|> bool
    <|> symbol
    <|> keyword
    <|> string
    <|> nil

parseNumber :: P.Parser Atom
parseNumber = P.try parseNum
  where
    parseNum :: P.Parser Atom
    parseNum = do
        s <- P.optionMaybe $ P.char '-'
        whole <- P.many1 P.digit
        c <- P.optionMaybe ( P.char '.')
        case c of
            Nothing -> (return . Integer . read) (sign s ++ whole)
            Just _  -> do
                part <- P.many1 P.digit
                (return . Real . read) (sign s ++ whole ++ "." ++ part)

    sign :: Maybe a -> String
    sign s = case s of { Nothing -> ""; Just _ -> "-" }

symbol :: P.Parser Atom
symbol = Symbol <$> L.identifier

keyword :: P.Parser Atom
keyword = do
  _ <- P.char ':'
  Keyword<$> L.identifier

escapeChar :: P.Parser Char
escapeChar = P.char '\\' >> P.choice (zipWith escapedChar codes replacements)
  where escapedChar code replacement = P.char code >> return replacement
        codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

bool :: P.Parser Atom
bool = do
  _ <- P.char '#'
  P.oneOf "ft" >>= \c -> return $ case c of
    't' -> Bool True
    'f' -> Bool False
    _   -> undefined -- to silence exahustiveness warning

string :: P.Parser Atom
string = do
    _ <- P.char '"'
    str <- P.many (P.try escapeChar <|> P.try (P.noneOf "\""))
    _ <- P.char '"'
    pure $ String str

nil :: P.Parser Atom
nil = L.reserved "nil" >> pure Nil

-----------------
-- Expressions
-----------------

withMD :: P.Parser a -> P.Parser (WithMD a)
withMD parser =  P.getPosition >>= \pos -> WithMD pos <$> parser

quote :: P.Parser Expr -> P.Parser Expr
quote e = do
  WithMD md _ <- withMD $ P.char '\''
  res <- withMD e
  return $ LIST [WithMD md (ATOM $ Symbol "quote"), res]

expr :: P.Parser Expr
expr = quote expr
    <|> (LIST <$> list)
    <|> (ATOM <$> atom)

list :: P.Parser [WithMD Expr]
list  = parensOrBrackets $ P.sepBy (withMD expr) P.spaces

-------------
-- Modules
-------------

modules :: P.Parser [WithMD ModuleDef]
modules = do
  mods <- P.many moduleDef
  P.eof
  return mods

moduleDef :: P.Parser (WithMD ModuleDef)
moduleDef = withMD $ do
  (fName, mName, exposes) <- modDef
  reqs <- requires
  defs <- P.many1 define
  return $ ModuleDef fName mName exposes reqs defs

modDef :: P.Parser (P.SourceName, Name, Maybe [Name])
modDef = do
  fName <- fmap P.sourceName P.getPosition
  L.reserved "(module"
  P.spaces
  mName <- L.identifier
  P.spaces
  exposes <- P.optionMaybe $ parensOrBrackets $ P.sepBy L.identifier P.spaces
  rparen
  return (fName, mName, exposes)


-- Defines

define :: P.Parser (Name, WithMD Expr)
define =  do
  L.reserved "(define"
  name <- L.identifier
  P.spaces
  expression <- P.try defineFun <|> withMD expr
  rparen
  return (name, expression)

ignoreCharSpaces :: Char -> P.Parser ()
ignoreCharSpaces c = do
  P.spaces
  void $ L.symbol [c]
  P.spaces

rparen :: P.Parser ()
rparen = ignoreCharSpaces ')'
--lparen :: P.Parser ()
--lparen = ignoreCharSpaces '('

defineFun :: P.Parser (WithMD Expr)
defineFun = withMD $ do
  md <- P.getPosition
  WithMD argsMD args <- funArgs
  body <- withMD expr
  pure $ LIST [WithMD md (ATOM (Symbol "lambda")), WithMD argsMD args, body]

funArgs :: P.Parser (WithMD Expr)
funArgs = withMD $
  P.try funArgList <|> fmap ATOM symbol

funArgList :: P.Parser Expr
funArgList = do
  args <- parensOrBrackets $ P.sepBy (withMD argSymbol) P.spaces
  case validArgs args of
      Just _  -> P.parserFail "unexpected &. rest argument is not last."
      Nothing -> return $ (LIST . map (fmap ATOM)) args

validArgs :: [WithMD Atom] -> Maybe Metadata
validArgs [] = Nothing
validArgs (x:xs) = case x of
  WithMD md (Symbol ('&':_)) -> if null xs then Nothing else Just md
  _ -> validArgs xs

argSymbol :: P.Parser Atom
argSymbol = do
  restTag <- P.optionMaybe (P.char '&')
  lazyTag <- P.optionMaybe (P.char '~')
  name <- L.identifier
  return $ case (restTag, lazyTag) of
    (Nothing, Nothing) -> Symbol name
    (Just rt, Just lt) -> Symbol (rt:lt:name)
    (Just rt, _      ) -> Symbol (rt:name)
    (_      , Just lt) -> Symbol (lt:name)


-- requires
requires :: P.Parser [Require]
requires = P.many require

require :: P.Parser Require
require =  do
  fName <- fmap P.sourceName P.getPosition
  L.reserved "(require"
  String modFilePath <- fromMaybe (String fName) <$> P.optionMaybe string
  P.spaces
  moduleName <- L.identifier
  P.spaces
  newName <- P.optionMaybe L.identifier
  P.spaces
  args    <- P.optionMaybe $ parensOrBrackets $ P.sepBy L.identifier P.spaces
  rparen
  return $ Require modFilePath moduleName newName args

-------------
-- Parsing
-------------

lineComment :: P.Parser Char
lineComment = do
  _ <- P.oneOf ";"
  _ <- P.manyTill P.anyChar P.newline
  return ';'

nestedComments :: P.Parser Char
nestedComments  = do
  _ <- P.string "{~"
  _ <- P.manyTill (P.try nestedComments <|> P.anyChar) (P.try (P.string "~}"))
  return ';'

comments :: P.Parser Char
comments = (P.try P.space >> pure ';') <|> P.try lineComment <|> P.try nestedComments

parseExpr :: String -> String -> Either String (WithMD Expr)
parseExpr name input = case P.parse (P.many comments >> withMD expr) name input of
  Left err -> Left (show err)
  Right val -> Right val

parseFile :: String -> String -> Either String [WithMD ModuleDef]
parseFile name input = case P.parse (P.many comments >> modules) name input of
  Left err  -> Left (show err)
  Right modefs ->
    forM modefs (\modef ->
      let dups = duplicates $ map fst (modDefs $ stripMD modef)
      in
        if   null dups
        then Right modef
        else Left $ unwords ["module",  modName $ stripMD modef, "in file", name, "has duplicate definitions of:", show dups])

parseReqDefExp :: String -> String -> Either String ReqDefExp
parseReqDefExp name input =
  case P.parse ((Req <$> require) <|> (Def <$> define) <|> (Exp <$> withMD expr)) name input of
    Left  err -> Left (show err)
    Right val -> Right val

getMDSource :: Metadata -> String
getMDSource = P.sourceName
