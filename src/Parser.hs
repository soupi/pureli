{-# LANGUAGE LambdaCase #-}

module Parser (parseExpr, parseFile) where

import Control.Applicative (pure, (<$>))

import Data.Either (partitionEithers)
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P (Parser)

import AST
import Printer()
import qualified Lexer as L
import ParseNumber

parensOrBrackets :: P.Parser a -> P.Parser a
parensOrBrackets parser = L.parens parser <|> L.brackets parser


-----------
-- Atoms
-----------

atom :: P.Parser Atom
atom =  parseNumber
    <|> bool
    <|> symbol
    <|> string
    <|> nil

symbol :: P.Parser Atom
symbol = Symbol <$> L.identifier

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

expr :: P.Parser Expr
expr =  (LIST <$> list)
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
  (regDefs, macroDefs) <- defines
  return $ ModuleDef fName mName exposes reqs macroDefs regDefs

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
  L.symbol [c]
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
  args <- parensOrBrackets $ P.sepBy (withMD symbol) P.spaces
  case validArgs args of
      Just _  -> P.parserFail "unexpected &. rest argument is not last."
      Nothing -> return $ (LIST . map (fmap ATOM)) args

validArgs :: [WithMD Atom] -> Maybe Metadata
validArgs [] = Nothing
validArgs (x:xs) = case x of
  WithMD md (Symbol ('&':_)) -> if null xs then Nothing else Just md
  _ -> validArgs xs


defines :: P.Parser ([(Name, WithMD Expr)], [(Name, WithMD Expr)])
defines = fmap partitionEithers go
  where go = P.many1 df
        df = fmap Left define <|> fmap Right defmacro

-- Macros

defmacro :: P.Parser (Name, WithMD Expr)
defmacro =  do
  pos <- P.getPosition
  L.reserved "(defmacro"
  name <- L.identifier
  expression <- P.many1 macroArgs
  rparen
  return (name, WithMD pos (LIST expression))

macroArgs :: P.Parser (WithMD Expr)
macroArgs = parensOrBrackets $ withMD $ do
  WithMD argsMD args <- funArgs
  body <- withMD expr
  pure $ LIST [WithMD argsMD args, body]

-- requires
requires :: P.Parser [Require]
requires = P.many require

require :: P.Parser Require
require =  do
  L.reserved "(require"
  String modFilePath <- string
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
  Left err -> Left (show err)
  Right val -> Right val
