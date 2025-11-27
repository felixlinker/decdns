{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module ZoneFile
  ( Name(..)
  , Cat(..)
  , isSubDomain
  , dnameRewrite
  , Type(..)
  , RData(..)
  , Record(..)
  , Node(..)
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

data Type = CNAME | DNAME | NS | A | TXT | MX | SOA
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
  { rname :: Name a
  , rrdata :: RData }
  deriving (Eq, Ord)

instance Show (Record String) where
  show (Record n (RData c ttl t d)) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

data Node a = Node
  { label :: a
  , records :: [RData]
  , children :: M.Map a (Node a) }

data Zone a = Zone
  { zoneName :: Name a
  , root :: Node a }

insert :: Ord a => Zone a -> Record a -> Maybe (Zone a)
insert (Zone norg@(Name org) zroot) (Record (Name n) d) =
  let rPath = pathInZone (reverse $ NE.toList org) (reverse $ NE.toList n)
  in do
    p <- rPath
    return $ Zone norg (go p zroot)
  where
    pathInZone :: Eq a => [a] -> [a] -> Maybe [a]
    pathInZone [] bs = Just bs
    pathInZone _ [] = Nothing
    pathInZone (a:as) (b:bs)
      | a == b = if null as then Just bs else pathInZone as bs
      | otherwise = Nothing

    go :: Ord a => [a] -> Node a -> Node a
    go [] node@(Node _ rs _) = node { records = d:rs }
    go (lr:t) node@(Node _ _ ch) =
      let emptyNode = Node lr [] M.empty
          chf = if M.member lr ch
            then M.update (Just . go t) lr
            else M.insert lr (go t emptyNode)
      in node { children = chf ch }

allRecords :: Zone a -> [Record a]
allRecords (Zone (Name orig) ns) = go orig ns
  where
    go :: NE.NonEmpty a -> Node a -> [Record a]
    go path (Node l rs ch) =
      let path' = l NE.<| path
      in map (Record (Name path')) rs ++ concatMap (go path') (M.elems ch)
