{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module ZoneFile
  ( Name(..)
  , Cat(..)
  , isSubDomain
  , dnameRewrite
  , Type(..)
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

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA
  deriving (Eq, Ord, Show)

data Cat = IN
  deriving (Eq, Ord, Show)

data RData = RData
  { rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type
  , rdata :: [String] }
  deriving (Eq, Ord)

data Record a = Record
  { rname :: a
  , rrdata :: RData }
  deriving (Eq, Ord)

instance Show a => Show (Record a) where
  show (Record n (RData c ttl t d)) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

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
allRecords (Zone (Name orig) ns) = go orig ns
  where
    go :: NE.NonEmpty a -> Node a -> [Record (Name a)]
    go path (Node rs ch) =
      map (Record (Name path)) rs ++ concat (M.mapWithKey (\l -> go (l NE.<| path)) ch)
