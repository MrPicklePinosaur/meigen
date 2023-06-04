module Parser where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.String.Combinator (many1, choice, chainl1)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Text.Parsec.Char (string)

myParser = pExpr

data Expr = Num Integer
    | Var String
    | Add Expr Expr
    | Paren Expr
    deriving (Show, Eq)

-- Always consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Consume leading whitespace
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof $ whitespace *> p

pExpr :: Parser Expr
pExpr = chainl1 pTerm (Add <$ pChar '+')

pTerm :: Parser Expr
pTerm = pNum <|> pVar <|> pParen pExpr

-- Parse Num
pNum :: Parser Expr
pNum = Num <$> pInt

pInt :: Parser Integer
pInt = read <$> lexeme (many1 digit)

-- Parse Var
pVar :: Parser Expr
pVar = Var <$> pIden

pIden :: Parser String
pIden = lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where
        firstChar = letter <|> char '_'
        nonFirstChar = letter <|> digit <|> char '_'

-- Parse Paren
pParen :: Parser Expr -> Parser Expr
pParen inner = Paren <$> (pChar '(' *> inner <* pChar ')')

pChar :: Char -> Parser Char
pChar = lexeme <$> char

-- Parse Add
pAdd :: Parser Expr
pAdd = Add <$> l <*> r
    where
        l = pTerm <* pChar '+'
        r = pExpr

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

