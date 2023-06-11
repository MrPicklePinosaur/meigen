module SimpleParser where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.String.Combinator (many1, choice, chainl1, between, notFollowedBy)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, guard)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing
import Text.Parsec.Char (string)

import qualified Test.Hspec as H

import qualified Text.Parsec.String.Expr as E

myParser = pExpr

data BinOp = Add | Sub | Mult | Div | Less | LessEq | Great | GreatEq | Eq
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
exprTable = [unOp Pos "+"
            ,unOp Neg "-"
            ,binOp LessEq "<=" E.AssocLeft
            ,binOp Less "<" E.AssocLeft
            ,binOp GreatEq ">=" E.AssocLeft
            ,binOp Great ">" E.AssocLeft
            ,binOp Mult "*" E.AssocLeft
            ,binOp Div "/" E.AssocLeft
            ,binOp Add "+" E.AssocLeft
            ,binOp Sub "-" E.AssocLeft
             ]
    where
        binOp :: BinOp -> String -> E.Assoc -> [E.Operator Expr]
        binOp op sym assoc = [E.Infix (Binary op <$ pSym sym) assoc]

        unOp :: UnOp -> String -> [E.Operator Expr]
        unOp op sym = [E.Prefix (Unary op <$ pSym sym)]

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

pSym :: String -> Parser String
pSym s = try $ lexeme $ do
    void $ string s
    -- TODO should dynamically generate list of characters that make up symbols?
    notFollowedBy (oneOf "<>=+-^%/*")
    return s

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- tests

-- operatorTests :: [(String, Expr)]
-- operatorTests = [("1 < 2", (Binary Less (Num 1) (Num 2)))]

-- runTests :: (Eq a, Show a) => Parser a -> (String, a) -> H.Test
-- runTests parser (input, expected) = H.TestLabel input $ H.TestCase $ do

