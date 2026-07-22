{-# LANGUAGE FlexibleContexts #-}
module Parsing.Name
  ( absName
  , relName
  , name
  , parseName
  , parseNameM ) where

import Data.Functor (($>))
import Control.Monad (MonadPlus (mzero), when)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Text.Parsec
import Text.Parsec.Char as Char

import DNS.Name (Name(..), root)
import Parsing.Tokens
import Parsing.State (OriginProvider (getOrigin))
import Data.Char (toLower)

origin :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
origin = do
  _ <- Char.char '@'
  org <- getOrigin <$> getState
  maybe (fail "no origin") return org

letDigHyp :: Stream s m Char => ParsecT s u m Char
letDigHyp = letDig <|> Char.char '-'

letDig :: Stream s m Char => ParsecT s u m Char
letDig = letter <|> digit

label :: Stream s m Char => ParsecT s u m String
label = string "*" <|> do
  p0 <- letDig
  p1 <- many letDigHyp
  p2 <- many letDig
  return $ map toLower $ p0 : p1 ++ p2

absName :: Stream s m Char => ParsecT s u m (Name String)
absName = rootName <|> nonRootName
  where
    nonRootName :: Stream s m Char => ParsecT s u m (Name String)
    nonRootName = do
      ls <- endBy1 Parsing.Name.label dot
      return $ Name $ NE.prependList ls $ NE.singleton ""

    rootName :: Stream s m Char => ParsecT s u m (Name String)
    rootName = dot $> root

relName :: (Stream s m Char) => ParsecT s u m (Name String)
relName = do
  ls <- sepBy1 Parsing.Name.label dot
  maybe (fail "no labels parsed") (return . Name) (NE.nonEmpty ls)

relToAbsName :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
relToAbsName = do
  ls <- sepBy1 Parsing.Name.label dot
  Just (Name org) <- getOrigin <$> getState
  return $ Name $ NE.prependList ls org

name :: (Stream s m Char, OriginProvider u) => ParsecT s u m (Name String)
name = choice [origin, try absName, relToAbsName]

skipPath :: Stream s m Char => ParsecT s u m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) (Just (root :: Name String)) "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return
