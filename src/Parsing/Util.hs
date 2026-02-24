{-# LANGUAGE FlexibleContexts #-}
module Parsing.Util
  ( manyTill1 ) where

import Text.Parsec
import Control.Monad (guard)

manyTill1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
manyTill1 p end = do
  parsed <- manyTill p end
  guard (not $ null parsed)
  return parsed

