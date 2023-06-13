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

type FnId = String
type VarId = String

data Fn = Fn {
    name :: FnId,
    argList :: [String],
    body :: Stmt
}
    deriving (Show, Eq)

data Stmt = If Expr Expr Expr | Expr Expr
    deriving (Show, Eq)


data BinOp = Add | Sub | Mult | Div
    deriving (Show, Eq)

data UnOp = Pos | Neg
    deriving (Show, Eq)

data Expr = Binary BinOp Expr Expr | Num Integer | Str String | Boolean Bool | Var VarId
    deriving (Show, Eq)

myParser :: Parser Fn
myParser = pFn

pFn :: Parser Fn
pFn = do
    name <- pFnId
    void $ pSynString "とは"
    body <- pStmt
    void $ pSynString "のことです"
    return Fn {
        name = name,
        argList = [],
        body = body
    }

pFnId :: Parser String
pFnId = many1 isKanji

pStmt :: Parser Stmt
pStmt = try pIf <|> Expr <$> pExpr

exprTable :: [[E.Operator Expr]]
exprTable = [binOp Add "プラス" E.AssocLeft
            ,binOp Sub "マイナス" E.AssocLeft
             ]
    where
        binOp :: BinOp -> String -> E.Assoc -> [E.Operator Expr]
        binOp op sym assoc = [E.Infix (Binary op <$ pSynString sym) assoc]
        -- unOp :: UnOp -> String -> [E.Operator Expr]
        -- unOp op sym = [E.Prefix (Unary op <$ pSym sym)]

-- pLet :: Parser Expr
-- pLet = do
--     name <- pIdent
--     void $ pSynString "は"
--     body <- pExpr
--     void $ pSynString "です"
--     return $ Let name body

pIf :: Parser Stmt
pIf = do
    cond <- pExpr
    void $ pSynString "なら"
    trueBranch <- pExpr
    void $ pSynString "そうでなければ"
    falseBranch <- pExpr
    return $ If cond trueBranch falseBranch

pExpr :: Parser Expr
pExpr = E.buildExpressionParser exprTable pTerm

pTerm :: Parser Expr
pTerm = try pString <|> pNum <|> pBoolean <|> pVar

pBoolean :: Parser Expr
pBoolean = Boolean <$> (try pTrue <|> pFalse)

pVar :: Parser Expr
pVar = Var <$> pIdent

pIdent :: Parser String
pIdent = many1 isKatakana

pTrue :: Parser Bool
pTrue = pChar '陽' >> return True

pFalse :: Parser Bool
pFalse = pChar '陰' >> return False

pString :: Parser Expr
pString = Str <$> between (pChar '「') (pChar '」') (many1 alphaNum)

-- TODO, currently doesn't support numbers without a 1s digit
pNum :: Parser Expr
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
