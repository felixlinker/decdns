{-# LANGUAGE FlexibleContexts #-}
module Parsing.Name
  ( name
  , parseName
  , parseNameM ) where

import Control.Monad (MonadPlus (mzero), when)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Text.Parsec
import Text.Parsec.Char as Char

import ZoneFile
import Parsing.Tokens
import Parsing.State (OriginProvider (getOrigin))

splitName :: Maybe (Name String) -> String -> Maybe (Name String)
splitName origin "@" = origin
splitName _ "." = Just $ Name $ "" NE.:| []
splitName origin val = case span (/= '.') val of
  (lbl, "") -> do
    org <- origin
    Just (lbl <| org)
  (lbl, '.':"") -> Just $ Name $ lbl NE.:| [""]
  (lbl, '.':t) -> (lbl <|) <$> splitName origin t
  (_, _) -> Nothing

name :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
name = do
  n <- many1 (lower <|> digit <|> Char.char '-' <|> Char.char '@' <|> Char.char '*' <|> dot)
  origin <- getOrigin <$> getState
  maybe (fail "could not split name") return (splitName origin n)

skipPath :: Stream s m Char => ParsecT s u m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) () "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return
