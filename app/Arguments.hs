module Arguments
  ( Options(..)
  , parseArguments) where

import Options.Applicative

data Options = Options
  { timeout :: Int
  }

parseArguments :: [String] -> IO (Options, [String])
parseArguments arguments = do
  handleParseResult $ execParserPure defaultPrefs argumentParserInfo arguments

argumentParserInfo :: ParserInfo (Options, [String])
argumentParserInfo = info (helper <*> parser) $
  fullDesc
    <> progDesc "Compile DNS zone data and analyze resolvability."

parser :: Parser (Options, [String])
parser = (,) <$> optionsParser <*> many (strArgument (metavar "ARGS"))

optionsParser :: Parser Options
optionsParser = Options <$> option auto
  ( long "timeout"
  <> metavar "INTEGER"
  <> value (-1)
  <> showDefault
  <> help "Timeout value"
  )
