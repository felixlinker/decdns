{-# LANGUAGE FlexibleContexts #-}
module Parsing.Name
  ( name
  , parseName
  , parseNameM ) where

import Control.Monad (MonadPlus (mzero), when)
import Data.Functor.Identity (Identity(Identity))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Text.Parsec
import Text.Parsec.Char as Char

import ZoneFile
import Parsing.Tokens

splitName :: String -> Maybe (NE.NonEmpty String)
splitName "." = Just ("" NE.:| [])
splitName val = case span (/= '.') val of
  (lbl, "") -> Just (NE.singleton lbl)
  (lbl, '.':"") -> Just (lbl NE.:| [""])
  (lbl, '.':t) -> (lbl NE.<|) <$> splitName t
  (_, _) -> Nothing

name :: Stream s m Char => ParsecT s u m (Name String)
name = do
  n <- many1 (lower <|> digit <|> Char.char '-' <|> Char.char '@' <|> Char.char '*' <|> dot)
  maybe (fail "could not split name") (return . Name) (splitName n)

skipPath :: Stream s m Char => ParsecT s u m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) Identity "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return
