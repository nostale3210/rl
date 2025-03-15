module Main.Cli where

import Options.Applicative

data Command = Command
  { cmd :: ![String],
    sumFlag :: !Bool,
    medianFlag :: !Bool,
    addOption :: !Int,
    increaseOption :: !Int,
    keepHOption :: !Int,
    keepLOption :: !Int
  }

optsParser :: ParserInfo Command
optsParser =
  info
    (helper <*> commandOptions)
    ( fullDesc
        <> progDesc "Basic dice roller"
        <> header "Roll dice with(out) style"
        <> footer "Truly somewhat random"
    )

commandOptions :: Parser Command
commandOptions =
  Command
    <$> many (argument str (metavar "DICE"))
    <*> flag False True (long "sum" <> short 's' <> help "Calculate sum of roll")
    <*> flag False True (long "median" <> short 'm' <> help "Calculate the median of a dice roll")
    <*> option auto (long "add" <> short 'a' <> metavar "INT" <> value 0 <> help "Add INT to total of roll")
    <*> option auto (long "increase" <> short 'i' <> metavar "INT" <> value 0 <> help "Increase individual dice by INT")
    <*> option auto (long "kh" <> long "keep-highest" <> metavar "INT" <> value 0 <> help "Discard all dice except INT highest")
    <*> option auto (long "kl" <> long "keep-lowest" <> metavar "INT" <> value 0 <> help "Discard all dice except INT lowest")
