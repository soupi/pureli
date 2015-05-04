module Parser (getValFromExpr, parseFile) where

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
parensOrBrackets parser = P.try (L.parens parser) <|> P.try (L.brackets parser)


-----------
-- Atoms
-----------

atom :: P.Parser Atom
atom =  P.try parseNumber
    <|> P.try bool
    <|> P.try symbol
    <|> P.try string
    <|> P.try nil

symbol :: P.Parser Atom
symbol = Symbol <$> L.identifier

escapeChar :: P.Parser Char
escapeChar = P.char '\\' >> P.anyChar

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
expr =  (LIST   <$> P.try list)
    <|> (ATOM   <$> P.try atom)

list :: P.Parser [WithMD Expr]
list  = parensOrBrackets $ P.sepBy (withMD expr) P.spaces

-------------
-- Program
-------------

modules :: P.Parser [WithMD ModuleDef]
modules = P.many moduleDef

moduleDef :: P.Parser (WithMD ModuleDef)
moduleDef = withMD $ do
  (fName, mName, exposes) <- modDef
  reqs <- requires
  (regDefs, macroDefs) <- defines
  return $ ModuleDef fName mName exposes reqs macroDefs regDefs

modDef :: P.Parser (P.SourceName, Atom, Maybe [Name])
modDef = parensOrBrackets $ do
  fName <- fmap P.sourceName P.getPosition
  _ <- P.string "module"
  P.spaces
  mName <- symbol
  P.spaces
  exposes <- P.optionMaybe $ parensOrBrackets $ P.sepBy L.identifier P.spaces
  return (fName, mName, exposes)


-- Defines

define :: P.Parser (Name, WithMD Expr)
define =  parensOrBrackets $ do
  L.reserved "define"
  name <- L.identifier
  expression <- P.try defineFun <|> P.try (withMD expr)
  return (name, expression)


defineFun :: P.Parser (WithMD Expr)
defineFun = withMD $ do
  md <- P.getPosition
  WithMD argsMD args <- withMD $ parensOrBrackets $ P.sepBy (withMD symbol) P.spaces
  case validArgs args of
    Just _ -> P.parserFail "unexpected &. argument is not last."
    Nothing  -> do
      body <- withMD expr
      pure $ LIST [WithMD md (ATOM (Symbol "lambda")), WithMD argsMD (LIST (map (fmap ATOM) args)), body]

defines :: P.Parser ([(Name, WithMD Expr)], [(Name, WithMD Expr)])
defines = go >>= return . partitionEithers
  where go = P.many1 $ (P.try define >>= return . Left) <|> (P.try defmacro >>= return . Right)

validArgs :: [WithMD Atom] -> Maybe P.SourcePos
validArgs [] = Nothing
validArgs (x:xs) = case x of
  WithMD md (Symbol ('&':_)) -> if length xs == 0 then Nothing else Just md
  _ -> validArgs xs

-- Macros

defmacro :: P.Parser (Name, WithMD Expr)
defmacro =  parensOrBrackets $ do
  pos <- P.getPosition
  L.reserved "defmacro"
  name <- L.identifier
  expression <- P.many1 macroArgs
  return (name, WithMD pos (LIST expression))

macroArgs :: P.Parser (WithMD Expr)
macroArgs = parensOrBrackets $ withMD $ do
  WithMD argsMD args <- withMD $ parensOrBrackets $ P.sepBy (withMD symbol) P.spaces
  body <- withMD expr
  pure $ LIST [WithMD argsMD (LIST (map (fmap ATOM) args)), body]

-- requires
requires :: P.Parser [Require]
requires = P.many require

require :: P.Parser Require
require =  parensOrBrackets $ do
  L.reserved "require"
  String modFilePath <- string
  P.spaces
  moduleName <- L.identifier
  P.spaces
  newName <- P.optionMaybe L.identifier
  P.spaces
  args    <- P.optionMaybe $ parensOrBrackets $ P.sepBy L.identifier P.spaces
  P.spaces
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

getValFromExpr :: String -> String -> Either String (WithMD Expr)
getValFromExpr name input = case P.parse (P.many comments >> withMD expr) name input of
  Left err -> Left (show err)
  Right val -> Right val

parseFile :: String -> String -> Either String [WithMD ModuleDef]
parseFile name input = case P.parse (P.many comments >> modules) name input of
  Left err -> Left (show err)
  Right val -> Right val
