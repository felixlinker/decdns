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

import DNS.Name (Name(..))
import Parsing.Tokens
import Parsing.State (OriginProvider (getOrigin))

name :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
name = origin <|> absName <|> relName
  where
    absName :: Stream s m Char => ParsecT s u m (Name String)
    absName = do
      ls <- endBy1 lbl dot
      return $ Name $ NE.fromList $ ls ++ [""]

    relName :: Stream s m Char => ParsecT s u m (Name String)
    relName = do
      ls <- sepBy1 lbl dot
      return $ Name $ NE.fromList ls

    lbl :: Stream s m Char => ParsecT s u m String
    lbl = do
      p0 <- letter
      p1 <- many letDigHyp
      p2 <- many letDig
      return $ p0 : p1 ++ p2

    letDigHyp :: Stream s m Char => ParsecT s u m Char
    letDigHyp = letDig <|> Char.char '-'

    letDig :: Stream s m Char => ParsecT s u m Char
    letDig = letter <|> digit

    origin :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
    origin = do
      _ <- Char.char '@'
      org <- getOrigin <$> getState
      maybe (fail "no origin") return org

skipPath :: Stream s m Char => ParsecT s u m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) () "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return
