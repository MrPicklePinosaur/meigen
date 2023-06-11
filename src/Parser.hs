module Parser where

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
import qualified Data.Map as M

import qualified Text.Parsec.String.Expr as E
import Text.Parsec (optionMaybe)
import Data.Maybe (fromMaybe)
import Text.Parsec (option)

data Literal = Num Integer

-- pNum :: String -> Parser Integer
-- pNum =

-- myParser = kDigit
myParser = pNum

pNum :: Parser Integer
pNum = kDigitHundred

-- Takes in a 'base' character, 'base' value as well as the next parser to use
kDigitBuilder :: Char -> Integer -> Parser Integer -> Parser Integer
kDigitBuilder ch base next = try include <|> skip
    where
        include = do
            left <- option 1 kDigit
            void $ pChar ch
            right <- next
            return $ left * base + right
        skip = next

kDigitHundred :: Parser Integer
kDigitHundred = kDigitBuilder '百' 100 kDigitTen

kDigitTen :: Parser Integer
kDigitTen = kDigitBuilder '十' 10 kDigitMaybe

kDigitMaybe :: Parser Integer
kDigitMaybe = try kDigit <|> return 0

-- Return zero if not matched
kDigit :: Parser Integer
kDigit = choice $ map _kDigit _digitLookup
    where
        _kDigit :: (Char, Integer) -> Parser Integer
        _kDigit (c, val) = satisfy (==c) >> return val
        _digitLookup :: [(Char, Integer)]
        _digitLookup = [
            ('一', 1),
            ('二', 2),
            ('三', 3),
            ('四', 4),
            ('五', 5),
            ('六', 6),
            ('七', 7),
            ('八', 8),
            ('九', 9)
          ]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- Always consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

pChar :: Char -> Parser Char
pChar = lexeme <$> char

-- Consume leading whitespace
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof $ whitespace *> p
