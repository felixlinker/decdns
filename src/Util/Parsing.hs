module Util.Parsing
  ( parseS ) where

import Control.Monad ((<=<))
import Text.Parsec

parseS :: MonadFail m => ParsecT String () m a -> String -> m a
parseS p = either (fail . show) return <=< runParserT p () "string parsing"
