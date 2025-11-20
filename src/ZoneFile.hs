{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module ZoneFile
  ( Name(..)
  , isSubDomain
  , dnameRewrite
  , name
  , recordType
  , Type(..)
  , Zone(..)
  , Record(..)
  , Parser
  , emptyState
  , parseName
  , parseNameM
  , parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Text.Read (readMaybe)
import Control.Monad (guard, MonadPlus (mzero), when)
import Control.Applicative (asum)
import qualified Control.Lens as L
import Data.Functor (($>))
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (isJust, catMaybes)

newtype Name = Name { labels :: [String] }
  deriving (Eq, Ord)

instance Show Name where
  show (Name ls) = intercalate "." ls

isSubDomain :: Name -> Name -> Bool
isSubDomain (Name l1) (Name l2) = l2 `isSuffixOf` l1

dnameRewrite :: Name -> Name -> Name -> Name
dnameRewrite qn@(Name qname) fn@(Name from) (Name to) =
  let prefixLen = length qname - length from
  in if not (isSubDomain qn fn)
    then undefined
    else Name (take prefixLen qname ++ to)

data Record = Record
  { rname :: Name
  , rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type
  , rdata :: [String] }
  deriving (Eq, Ord)

instance Show Record where
  show (Record n c ttl t d) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

data Zone = Zone
  { root :: Name
  , records :: [Record] }

data Type = CNAME | DNAME | NS | A | TXT | MX | SOA
  deriving (Eq, Ord, Show)

data Cat = IN
  deriving (Eq, Ord, Show)

data ParserState = ParserState
  { _zoneOrigin :: Maybe Name
  , _lastName :: Maybe Name
  , _lastTTL :: Maybe Int
  , _lastCat :: Maybe Cat }

L.makeLenses ''ParserState

emptyState :: ParserState
emptyState = ParserState Nothing Nothing Nothing Nothing

type Parser s m = ParsecT s ParserState m

writeState :: Monad m => L.Lens' ParserState (Maybe a) -> a -> Parser s m ()
writeState l a = modifyState (L.set l (return a))

readState :: Monad m => L.Lens' ParserState (Maybe a) -> Parser s m a
readState l = do
  st <- getState
  maybe (fail "required state missig") return (L.view l st)

recordType :: Stream s m Char => Parser s m Type
recordType = asum $ map try
  [ string "CNAME" $> CNAME
  , string "DNAME" $> DNAME
  , string "NS" $> NS
  , string "A" $> A
  , string "TXT" $> TXT
  , string "MX" $> MX
  , string "SOA" $> SOA ]

spaces1 :: Stream s m Char => Parser s m ()
spaces1 = skipMany1 space

dot :: Stream s m Char => Parser s m Char
dot = Char.char '.'

comment :: Stream s m Char => Parser s m ()
comment = do
  spaces
  _ <- Char.char ';'
  _ <- manyTill anyChar (endOfLine $> () <|> eof)
  return ()

name :: Stream s m Char => Parser s m Name
name = do
  n <- many1 (lower <|> digit <|> Char.char '-' <|> Char.char '@' <|> dot)
  maybe (fail "could not split name") (return . Name) (splitName n)
  where
    splitName :: String -> Maybe [String]
    splitName val = case span (/= '.') val of
      (lbl, "") -> Just [lbl]
      (lbl, '.':t) -> (lbl :) <$> splitName t
      (_, _) -> Nothing

origin :: Stream s m Char => Parser s m Name
origin = do
  spaces
  _ <- string "$ORIGIN"
  spaces1
  org <- name
  writeState zoneOrigin org
  return org

relName :: Stream s m Char => Parser s m Name
relName = do
  Name ls <- name
  orig@(Name origLs) <- readState zoneOrigin
  guard (not (null origLs))
  return $ if ls == ["@"] then orig else Name (ls ++ origLs)

ttl :: Stream s m Char => Parser s m Int
ttl = do
  ttls <- many1 digit
  maybe (fail "cannot parse") return (readMaybe ttls)

category :: Stream s m Char => Parser s m Cat
category = string "IN" $> IN

tryOrState :: Monad m => Parser s m a -> L.Lens' ParserState (Maybe a) -> Parser s m a
tryOrState p l = tryP <|> readState l
  where
    tryP = do
      a <- try p
      writeState l a
      return a

setTTL :: Stream s m Char => Parser s m ()
setTTL = do
  _ <- string "$TTL"
  spaces1
  t <- ttl
  writeState lastTTL t

record :: Stream s m Char => Parser s m Record
record = do
  spaces
  n <- tryOrState relName lastName
  spaces
  (c, t, ty) <- try catTtlType <|> try ttlCatType
  spaces1
  d <- manyTill anyChar end
  return (Record n c t ty (words d))
  where
    end :: Stream s m Char => Parser s m ()
    end = endOfLine $> () <|> eof

catTtlType :: Stream s m Char => Parser s m (Cat, Int, Type)
catTtlType = do
  c <- tryOrState category lastCat
  spaces
  t <- tryOrState ttl lastTTL
  spaces1
  ty <- recordType
  return (c, t, ty)

ttlCatType :: Stream s m Char => Parser s m (Cat, Int, Type)
ttlCatType = do
  t <- tryOrState ttl lastTTL
  spaces
  c <- tryOrState category lastCat
  spaces1
  ty <- recordType
  return (c, t, ty)

line :: Stream s m Char => Parser s m (Maybe Record)
line = spaces >> asum (map try
  [ comment $> Nothing
  , setTTL $> Nothing
  , Just <$> record ])

zone :: Stream s m Char => Parser s m Zone
zone = do
  org <- origin
  rs <- catMaybes <$> many line
  return $ Zone org rs

zoneFile :: Stream s m Char => Parser s m [Zone]
zoneFile = many zone

skipPath :: Stream s m Char => Parser s m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError Name)
parseName = runParserT (skipPath >> name) emptyState "parseName function"

parseNameM :: MonadPlus m => String -> m Name
parseNameM s = parseName s >>= either (const mzero) return

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone])
parseZoneFile = runParserT zoneFile emptyState
