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
import Text.Parsec (anyChar)
import Text.Parsec (alphaNum)

-- data Program = 

data Fn = Fn {
    name :: String,
    argList :: [String],
    body :: Expr
}
    deriving (Show, Eq)

data Expr = Lit Literal | If Expr Expr Expr
    deriving (Show, Eq)

data FnId = String

data Literal = Num Integer | Str String | Boolean Bool | Var String
    deriving (Show, Eq)

myParser :: Parser Fn
myParser = pFn

pFn :: Parser Fn
pFn = do
    name <- pFnId
    void $ pSynString "とは"
    body <- pExpr
    void $ pSynString "のことです"
    return Fn {
        name = name,
        argList = [],
        body = body
    }

pFnId :: Parser String
pFnId = many1 isKanji

pExpr :: Parser Expr
pExpr = try pIf <|> (Lit <$> pLit)

pIf :: Parser Expr
pIf = do
    cond <- pExpr
    void $ pSynString "なら"
    trueBranch <- pExpr
    void $ pSynString "そうでなければ"
    falseBranch <- pExpr
    return $ If cond trueBranch falseBranch

-- pLet :: Parser Expr
-- pLet = do
--     name <- pIdent
--     void $ pSynString "は"
--     body <- pExpr
--     void $ pSynString "です"
--     return $ Let name body

pLit :: Parser Literal
pLit = try pString <|> pNum <|> pBoolean <|> pVar

pBoolean :: Parser Literal
pBoolean = Boolean <$> (try pTrue <|> pFalse)

pVar :: Parser Literal
pVar = Var <$> pIdent

pIdent :: Parser String
pIdent = many1 isKatakana

pTrue :: Parser Bool
pTrue = pChar '陽' >> return True

pFalse :: Parser Bool
pFalse = pChar '陰' >> return False

pString :: Parser Literal
pString = Str <$> between (pChar '「') (pChar '」') (many1 alphaNum)

-- TODO, currently doesn't support numbers without a 1s digit
pNum :: Parser Literal
pNum = Num <$> kDigitThousand

-- Parse a string (for use in langauge syntax)
pSynString  :: String -> Parser String
pSynString = string

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

kDigitThousand :: Parser Integer
kDigitThousand = kDigitBuilder '千' 1000 kDigitHundred

kDigitHundred :: Parser Integer
kDigitHundred = kDigitBuilder '百' 100 kDigitTen

kDigitTen :: Parser Integer
kDigitTen = kDigitBuilder '十' 10 kDigit

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

isKanji :: Parser Char
isKanji = satisfy (\c -> (0x3400 <= fromEnum c && fromEnum c <= 0x4DB5) || (0x4E00 <= fromEnum c && fromEnum c <= 0x9FCB) || (0xF900 <= fromEnum c && fromEnum c <= 0xFA6A))

isKatakana :: Parser Char
isKatakana = oneOf "ァ ア ィ イ ゥ ウ ェ エ ォ オ カ ガ キ ギ ク グ ケ ゲ コ ゴ サ ザ シ ジ ス ズ セ ゼ ソ ゾ タ ダ チ ヂ ッ ツ ヅ テ デ ト ド ナ ニ ヌ ネ ノ ハ バ パ ヒ ビ ピ フ ブ プ ヘ ベ ペ ホ ボ ポ マ ミ ム メ モ ャ ヤ ュ ユ ョ ヨ ラ リ ル レ ロ ヮ ワ ヰ ヱ ヲ ン ヴ ヵ ヶ ヷ ヸ ヹ ヺ・ー"

isHiragana :: Parser Char
isHiragana = oneOf "ぁ あ ぃ い ぅ う ぇ え ぉ お か が き ぎ く ぐ け げ こ ご さ ざ し じ す ず せ ぜ そ ぞ た だ ち ぢ っ つ づ て で と ど な に ぬ ね の は ば ぱ ひ び ぴ ふ ぶ ぷ へ べ ぺ ほ ぼ ぽ ま み む め も ゃ や ゅ ゆ ょ よ ら り る れ ろ ゎ わ ゐ ゑ を ん ゔ ゕ ゖ"
