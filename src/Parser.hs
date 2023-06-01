module Parser where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, satisfy)
import Text.Parsec.String.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Text.Parsec.Char (string)

myParser = add

num1 :: Parser Integer
num1 = do
    n <- many1 digit
    return (read n)

var :: Parser String
var = do
    x <- firstChar
    xs <- many nonFirstChar
    return (x:xs)
    where
        firstChar = satisfy (\c -> isLetter c || c == '_')
        nonFirstChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

-- Always consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

-- Consume leading whitespace
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
    where
        wrapper = do
            whitespace
            p

data Paren = Paren Integer
    deriving (Show, Eq)

paren :: Parser Paren
paren = do
    void $ lexeme $ char '('
    contents <- lexeme $ many1 digit
    void $ lexeme $ char ')'
    return (Paren $ read contents)

data Add = Add Integer Integer
    deriving (Show, Eq)

add :: Parser Add
add = do
    l <- lexeme $ many1 digit
    void $ lexeme $ char '+'
    r <- lexeme $ many1 digit
    return (Add (read l) (read r))

whitespace :: Parser ()
whitespace = do
    void $ many $ oneOf " \n\t"

