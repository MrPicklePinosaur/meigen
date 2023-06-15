module Main where

import System.Environment
import System.IO

import Text.Parsec
import Text.Parsec.String
import Control.Monad

-- import SimpleParser (myParser, parseWithWhitespace)
import Parser (myParser, parseWithWhitespace)
import Cli (run, Cli)
import qualified Cli

main :: IO ()
main = handle =<< run
    -- a <- getArgs
    -- case a of
    --   [str] -> either print print $ parseWithWhitespace myParser str
    --   _ -> error "please pass one argument with the string to parse"

handle :: Cli -> IO ()
handle c = do
    fileContents <- readFileContents (Cli.file c)
    either print print $ parseWithWhitespace myParser fileContents

readFileContents :: String -> IO String
readFileContents filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents
