module Cli where

import Options.Applicative

data Cli = Cli
    { file :: String
    , verbose :: Bool
    }

argparse :: Parser Cli
argparse = Cli
    <$> strOption
        ( long "file"
        <> metavar "FILENAME"
        <> help "Run from file"
        )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Show more infomration")

run :: IO Cli
run = execParser opts
    where
        opts = info (argparse <**> helper)
            ( fullDesc
            <> progDesc "meigen programming language"
            <> header "meigen" )
