{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( spaces1
  , dot
  , end
  , untilEndOfLine
  , int
  , category ) where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Char as Char

import ZoneFile
import Text.Read (readMaybe)

spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = skipMany1 space

dot :: Stream s m Char => ParsecT s u m Char
dot = Char.char '.'

end :: Stream s m Char => ParsecT s u m ()
end = endOfLine $> () <|> eof

untilEndOfLine :: Stream s m Char => ParsecT s u m String
untilEndOfLine = manyTill anyChar end

int :: Stream s m Char => ParsecT s u m Int
int = do
  ttls <- many1 digit
  maybe (fail "cannot parse int") return (readMaybe ttls)

category :: Stream s m Char => ParsecT s u m Cat
category = string "IN" $> IN
