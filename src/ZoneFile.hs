{-# LANGUAGE FlexibleContexts #-}
module ZoneFile
  ( Name(..)
  , isSubDomain
  , dnameRewrite
  , name
  , recordType
  , Type(..)
  , Record(..)
  , Parser
  , parseName
  , parseNameM
  , parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Control.Monad (guard, MonadPlus (mzero), when)
import Control.Applicative (asum)
import Data.Functor (($>))
import qualified Data.List as L
import Data.List (intercalate)
import Data.Maybe (isJust)

newtype Name = Name { labels :: [String] }
  deriving (Eq, Ord)

instance Show Name where
  show (Name ls) = intercalate "." ls

isSubDomain :: Name -> Name -> Bool
isSubDomain (Name l1) (Name l2) = l2 `L.isSuffixOf` l1

dnameRewrite :: Name -> Name -> Name -> Name
dnameRewrite qn@(Name qname) fn@(Name from) (Name to) =
  let prefixLen = length qname - length from
  in if not (isSubDomain qn fn)
    then undefined
    else Name (take prefixLen qname ++ to)

-- TODO: Integrate DNAME | MX | PTR | SOA | TXT
data Type = CNAME | DNAME | NS | A
  deriving (Eq, Ord, Show)

recordType :: Stream s m Char => Parser s m Type
recordType = asum $ map try
  [ string "CNAME" $> CNAME
  , string "DNAME" $> DNAME
  , string "NS" $> NS
  , string "A" $> A ]

type Parser s m = ParsecT s Name m

spaces1 :: Stream s m Char => Parser s m ()
spaces1 = skipMany1 space

dot :: Stream s m Char => Parser s m Char
dot = Char.char '.'

-- comment :: Stream s m Char => Parser s m ()
-- comment = do
--   spaces
--   _ <- Char.char ';'
--   _ <- manyTill anyChar endOfLine
--   return ()

name :: Stream s m Char => Parser s m Name
name = do
  n <- many1 (letter <|> digit <|> Char.char '-' <|> Char.char '@' <|> dot)
  maybe (fail "could not split name") (return . Name) (splitName n)
  where
    splitName :: String -> Maybe [String]
    splitName val = case span (/= '.') val of
      (lbl, "") -> Just [lbl]
      (lbl, '.':t) -> (lbl :) <$> splitName t
      (_, _) -> Nothing

origin :: Stream s m Char => Parser s m ()
origin = do
  spaces
  _ <- string "$ORIGIN"
  spaces1
  org <- name
  putState org
  return ()

data Record = Record
  { rname :: Name
  , rtype :: Type
  , rdata :: String }
  deriving (Eq, Ord)

instance Show Record where
  show (Record n t d) = show n ++ " " ++ show t ++ " " ++ d

relName :: Stream s m Char => Parser s m Name
relName = do
  Name ls <- name
  orig@(Name origLs) <- getState
  guard (not (null origLs))
  return $ if ls == ["@"] then orig else Name (ls ++ origLs)

ttl :: Stream s m Char => Parser s m String
ttl = many1 digit

record :: Stream s m Char => Parser s m Record
record = do
  _ <- spaces
  n <- relName
  _ <- spaces
  _ <- ttl
  _ <- spaces
  t <- recordType
  _ <- spaces
  d <- manyTill anyChar space
  return (Record n t d)

zone :: Stream s m Char => Parser s m [Record]
zone = do
  origin
  many record

zoneFile :: Stream s m Char => Parser s m [Record]
zoneFile = concat <$> many zone

skipPath :: Stream s m Char => Parser s m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError Name)
parseName = runParserT (skipPath >> name) (Name []) "parseName function"

parseNameM :: MonadPlus m => String -> m Name
parseNameM s = parseName s >>= either (const mzero) return

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Record])
parseZoneFile = runParserT zoneFile (Name [])
