module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad

-- import SimpleParser (myParser, parseWithWhitespace)
import Parser (myParser, parseWithWhitespace)

main :: IO ()
main = do
    a <- getArgs
    case a of
      [str] -> either print print $ parseWithWhitespace myParser str
      _ -> error "please pass one argument with the string to parse"
