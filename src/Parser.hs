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

myParser :: Parser ()
myParser = void paren

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

data Paren = Paren Integer
    deriving (Show, Eq)

paren :: Parser Paren
paren = do
    void $ char '('
    contents <- many1 digit
    void $ char ')'
    return (Paren $ read contents)
