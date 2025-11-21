{-# LANGUAGE FlexibleContexts #-}
module Parsing.Name
  ( name
  , relName
  , parseName
  , parseNameM ) where

import Text.Parsec
import Text.Parsec.Char as Char

import ZoneFile
import Control.Monad (MonadPlus (mzero), when)
import Parsing.Tokens
import Data.Maybe (isJust)
import Data.Functor.Identity (Identity(Identity))

name :: Stream s m Char => ParsecT s u m (Name String)
name = do
  n <- many1 (lower <|> digit <|> Char.char '-' <|> Char.char '@' <|> dot)
  maybe (fail "could not split name") (return . Name) (splitName n)
  where
    splitName :: String -> Maybe [String]
    splitName val = case span (/= '.') val of
      (lbl, "") -> Just [lbl]
      (lbl, '.':t) -> (lbl :) <$> splitName t
      (_, _) -> Nothing

relName :: Stream s m Char => ParsecT s u m (Name String)
relName = do
  Name ls <- name
  return $ Name $ if ls == ["@"] then [] else ls

skipPath :: Stream s m Char => ParsecT s u m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) Identity "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return
