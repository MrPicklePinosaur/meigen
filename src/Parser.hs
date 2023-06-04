module Parser where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.String.Combinator (many1, choice, chainl1, between)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Text.Parsec.Char (string)

import qualified Text.Parsec.String.Expr as E

myParser = pExpr

data BinOp = Add | Sub | Mult | Div
    deriving (Show, Eq)

data UnOp = Pos | Neg
    deriving (Show, Eq)

data Expr = Num Integer
    | Var String
    | Paren Expr
    | Binary BinOp Expr Expr
    | Unary UnOp Expr
    deriving (Show, Eq)

exprTable :: [[E.Operator Expr]]
exprTable = [unOp Pos '+'
            ,unOp Neg '-'
            ,binOp Mult '*' E.AssocLeft
            ,binOp Div '/' E.AssocLeft
            ,binOp Add '+' E.AssocLeft
            ,binOp Sub '-' E.AssocLeft
             ]

binOp :: BinOp -> Char -> E.Assoc -> [E.Operator Expr]
binOp op ch assoc = [E.Infix (Binary op <$ pChar ch) assoc]

unOp :: UnOp -> Char -> [E.Operator Expr]
unOp op ch = [E.Prefix (Unary op <$ pChar ch)]

pExpr :: Parser Expr
pExpr = E.buildExpressionParser exprTable pTerm

-- Always consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Consume leading whitespace
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof $ whitespace *> p

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
pParen inner = Paren <$> between (pChar '(') (pChar ')') inner

pChar :: Char -> Parser Char
pChar = lexeme <$> char

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

