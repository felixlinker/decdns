module Util.Parsing
  ( handleParser
  , parseS ) where

import Control.Monad ((<=<))
import Text.Parsec

handleParser :: MonadFail m => Either ParseError a -> m a
handleParser = either (fail . show) return

parseS :: MonadFail m => ParsecT String () m a -> String -> m a
parseS p = handleParser <=< runParserT p () "string parsing"
