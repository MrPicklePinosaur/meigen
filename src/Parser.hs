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

myParser = pExpr

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

data Expr = Num Integer
    | Var String
    | Add Expr Expr
    | Paren Expr
    deriving (Show, Eq)

pExpr = do
    t <- pTerm
    maybeAddSuffix t
        where
            addSuffix t0 = do
                void $ lexeme $ char '+'
                t1 <- pTerm
                maybeAddSuffix (Add t0 t1)
            -- attempt to add suffix but return original if it fails (backtrack)
            maybeAddSuffix t0 = addSuffix t0 <|> return t0


pTerm :: Parser Expr
pTerm = try pNum <|> pVar <|> pParen

-- Parse Num
pNum :: Parser Expr
pNum = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

-- Parse Var
pVar :: Parser Expr
pVar = lexeme $ do
    x <- firstChar
    xs <- many nonFirstChar
    return $ Var (x:xs)
    where
        firstChar = satisfy (\c -> isLetter c || c == '_')
        nonFirstChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

-- Parse Paren
pParen :: Parser Expr
pParen = do
    void $ lexeme $ char '('
    contents <- pExpr
    void $ lexeme $ char ')'
    return (Paren $ contents)

-- Parse Add
pAdd :: Parser Expr
pAdd = do
    -- TOOD currently right assoicative
    l <- pTerm
    void $ lexeme $ char '+'
    r <- pExpr
    return $ Add l r

whitespace :: Parser ()
whitespace = do
    void $ many $ oneOf " \n\t"

