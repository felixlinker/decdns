{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module ZoneFile
  ( Name(..)
  , (<|)
  , Cat(..)
  , isSubDomain
  , dnameRewrite
  , Type(..)
  , TypeData(..)
  , showData
  , dtype
  , RData(..)
  , Record(..)
  , Node(..)
  , emptyZone
  , Zone(..)
  , insert
  , allRecords ) where

import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

newtype Name a = Name { labels :: NE.NonEmpty a }
  deriving (Eq, Ord)

(<|) :: a -> Name a -> Name a
(<|) l (Name ls) = Name (l NE.<| ls)

instance Functor Name where
  fmap f (Name ls) = Name (fmap f ls)

instance Show (Name String) where
  show (Name ls) = intercalate "." (NE.toList ls)

isSubDomain :: Eq a => Name a -> Name a -> Bool
isSubDomain (Name l1) (Name l2) = NE.toList l2 `isSuffixOf` NE.toList l1

dnameRewrite :: Eq a => Name a -> Name a -> Name a -> Name a
dnameRewrite qn@(Name qname) fn@(Name from) (Name to) =
  let prefixLen = length qname - length from
  in if not (isSubDomain qn fn) || prefixLen < 0
    then undefined
    else Name (NE.prependList (NE.take prefixLen qname) to)

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA | DNSKEY
  deriving (Eq, Ord, Show)

data TypeData =
    RCNAME (Name String)
  | RDNAME (Name String)
  | RNS (Name String)
  | RA String
  | RAAAA String
  | RTXT String
  | RMX { _preference :: Int
        , _exchange :: Name String }
  | RSOA  { _mname :: Name String
          , _rname :: Name String
          , _serial :: Int
          , _refresh :: Int
          , _retry :: Int
          , _expire :: Int
          , _minimum :: Int }
  | RDNSKEY { _zoneKey :: Bool
            , _secureEntryPoint :: Bool
            , _algorithm :: Int
            , _publicKey :: String }  -- TODO: Make ByteString
  deriving (Eq, Ord)

showData :: TypeData -> String
showData (RCNAME n) = show n
showData (RDNAME n) = show n
showData (RNS n) = show n
showData (RA addr) = addr
showData (RAAAA addr) = addr
showData (RTXT addr) = addr
showData (RMX p n) = unwords [show p, show n]
showData (RSOA a b c d e f g) = unwords [show a, show b, show c, show d, show e, show f, show g]
showData (RDNSKEY zk sep alg pk) = flags ++ " 3 " ++ show alg ++ " " ++ pk
  where
    flags :: String
    flags
      | sep = "257"
      | zk = "256"
      | otherwise = "0"

instance Show TypeData where
  show t = show (dtype t) ++ " " ++ showData t

dtype :: TypeData -> Type
dtype (RCNAME _) = CNAME
dtype (RDNAME _) = DNAME
dtype (RNS _) = NS
dtype (RA _) = A
dtype (RAAAA _) = AAAA
dtype (RTXT _) = TXT
dtype (RMX {}) = MX
dtype (RSOA {}) = SOA
dtype (RDNSKEY {}) = DNSKEY

data Cat = IN
  deriving (Eq, Ord, Show)

data RData = RData
  { rcat  :: Cat
  , rttl  :: Int
  , rtype :: TypeData }
  deriving (Eq, Ord)

data Record a = Record
  { rname :: a
  , rrdata :: RData }
  deriving (Eq, Ord)

instance Show a => Show (Record a) where
  show (Record n (RData c ttl t)) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " "

data Node a = Node
  { records :: [RData]
  , children :: M.Map a (Node a) }

emptyNode :: Node a
emptyNode = Node [] M.empty

data Zone a = Zone
  { zoneName :: Name a
  , root :: Node a }

emptyZone :: Name a -> Zone a
emptyZone org = Zone org emptyNode

pathInZone :: Eq a => Name a -> Name a -> Maybe [a]
pathInZone (Name (reverse . NE.toList -> org)) (Name (reverse . NE.toList -> n)) = pathInZone' org n
  where
    pathInZone' :: Eq a => [a] -> [a] -> Maybe [a]
    pathInZone' [] bs = Just bs
    pathInZone' _ [] = Just []
    pathInZone' (a:as) (b:bs)
      | a == b = pathInZone' as bs
      | otherwise = Nothing

insert :: Ord a => Zone a -> Record (Name a) -> Maybe (Zone a)
insert (Zone norg zroot) (Record n d) = do
  p <- pathInZone norg n
  return $ Zone norg (go p zroot)
  where
    go :: Ord a => [a] -> Node a -> Node a
    go [] node@(Node rs _) = node { records = d:rs }
    go (lr:t) node@(Node _ ch) =
      let chf = if M.member lr ch
            then M.update (Just . go t) lr
            else M.insert lr (go t emptyNode)
      in node { children = chf ch }

allRecords :: Zone a -> [Record (Name a)]
allRecords (Zone orig ns) = go orig ns
  where
    go :: Name a -> Node a -> [Record (Name a)]
    go n (Node rs ch) =
      map (Record n) rs ++ concat (M.mapWithKey (\l -> go (l <| n)) ch)
