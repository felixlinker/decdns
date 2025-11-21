{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module ZoneFile
  ( Name(..)
  , isSubDomain
  , dnameRewrite
  , name
  , recordType
  , Type(..)
  , Record(..)
  , Zone(..)
  , allRecords
  , Parser
  , emptyState
  , parseName
  , parseNameM
  , parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Text.Read (readMaybe)
import Control.Monad (MonadPlus (mzero), when)
import Control.Applicative (asum)
import qualified Control.Lens as L
import Data.Functor (($>))
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (isJust, catMaybes)
import qualified Data.Map as M

newtype Name a = Name { labels :: [a] }
  deriving (Eq, Ord)

instance Functor Name where
  fmap f (Name ls) = Name (fmap f ls)

instance Show a => Show (Name a) where
  show (Name ls) = intercalate "." (map show ls)

isSubDomain :: Eq a => Name a -> Name a -> Bool
isSubDomain (Name l1) (Name l2) = l2 `isSuffixOf` l1

dnameRewrite :: Eq a => Name a -> Name a -> Name a -> Name a
dnameRewrite qn@(Name qname) fn@(Name from) (Name to) =
  let prefixLen = length qname - length from
  in if not (isSubDomain qn fn)
    then undefined
    else Name (take prefixLen qname ++ to)

data Type = CNAME | DNAME | NS | A | TXT | MX | SOA
  deriving (Eq, Ord, Show)

data Cat = IN
  deriving (Eq, Ord, Show)

data Record a = Record
  { rname :: Name a
  , rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type
  , rdata :: [String] }
  deriving (Eq, Ord)

instance Show a => Show (Record a) where
  show (Record n c ttl t d) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

data Zone = Zone
  { root :: Name String
  , records :: [Record String]
  , children :: M.Map String Zone }

insert :: Zone -> Record String -> Zone
insert z_@(Zone org _ _) r_ = go (ZoneFile.labels org) r_ z_
  where
    go :: [String] -> Record String -> Zone -> Zone
    go acc r z@(Zone _ rs ch) = case L.unsnoc (ZoneFile.labels (rname r)) of
      Nothing -> Zone org (r { rname = Name acc }:rs) ch
      Just (ls, l) ->
        let rec = go ls (r { rname = Name ls })
            ch' = if M.member l ch
              then M.update (Just . rec) l ch
              else M.insert l (rec (Zone (Name [l]) [] M.empty)) ch
        in z { children = ch' }

allRecords :: Zone -> [Record String]
allRecords (Zone _ rs ch) = rs ++ concatMap allRecords (M.elems ch)

data ParserState = ParserState
  { _lastName :: Maybe (Name String)
  , _lastTTL :: Maybe Int
  , _lastCat :: Maybe Cat }

L.makeLenses ''ParserState

emptyState :: ParserState
emptyState = ParserState Nothing Nothing Nothing

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

name :: Stream s m Char => Parser s m (Name String)
name = do
  n <- many1 (lower <|> digit <|> Char.char '-' <|> Char.char '@' <|> dot)
  maybe (fail "could not split name") (return . Name) (splitName n)
  where
    splitName :: String -> Maybe [String]
    splitName val = case span (/= '.') val of
      (lbl, "") -> Just [lbl]
      (lbl, '.':t) -> (lbl :) <$> splitName t
      (_, _) -> Nothing

origin :: Stream s m Char => Parser s m (Name String)
origin = do
  spaces
  _ <- string "$ORIGIN"
  spaces1
  name

relName :: Stream s m Char => Parser s m (Name String)
relName = do
  Name ls <- name
  return $ Name $ if ls == ["@"] then [] else ls

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

record :: Stream s m Char => Parser s m (Record String)
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

line :: Stream s m Char => Parser s m (Maybe (Record String))
line = spaces >> asum (map try
  [ comment $> Nothing
  , setTTL $> Nothing
  , Just <$> record ])

zone :: Stream s m Char => Parser s m Zone
zone = do
  org <- origin
  rs <- catMaybes <$> many line
  return $ foldl insert (Zone org [] M.empty) rs

zoneFile :: Stream s m Char => Parser s m [Zone]
zoneFile = many zone

skipPath :: Stream s m Char => Parser s m ()
skipPath = do
  m <- optionMaybe (try $ manyTill (Char.noneOf ['/']) (Char.char '/'))
  when (isJust m) skipPath

parseName :: Monad m => String -> m (Either ParseError (Name String))
parseName = runParserT (skipPath >> name) emptyState "parseName function"

parseNameM :: MonadPlus m => String -> m (Name String)
parseNameM s = parseName s >>= either (const mzero) return

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone])
parseZoneFile = runParserT zoneFile emptyState
