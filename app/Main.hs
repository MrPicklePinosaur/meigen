module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad

import Parser (myParser)

main :: IO ()
main = do
    a <- getArgs
    case a of
      [str] -> either print print $ parse myParser "" str
      _ -> error "please pass one argument with the string to parse"
