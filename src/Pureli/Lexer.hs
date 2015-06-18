
-- |
-- This module defines how to lex the language
module Pureli.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        style = emptyDef { Tok.commentLine     = ";"
                         , Tok.reservedNames   = ["nil", "defmacro", "define", "require", "module", "(define", "(require", "(module", "#t", "#f"]
                         , Tok.caseSensitive   = True
                         , Tok.commentStart    = "{~"
                         , Tok.commentEnd      = "~}"
                         , Tok.nestedComments  = True
                         , Tok.identStart      = P.oneOf "&!$#%*+./<=>?@\\^|-~" <|> P.letter
                         , Tok.identLetter     = P.oneOf "!:$#%*+./<=>?@\\^|-~" <|> P.alphaNum
                         }

symbol :: String -> Parser String
symbol = Tok.symbol lexer

char :: Parser Char
char = Tok.charLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
