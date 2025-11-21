{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module ZoneFile
  ( Name(..)
  , Cat(..)
  , isSubDomain
  , dnameRewrite
  , Type(..)
  , Record(..)
  , Zone(..)
  , insert
  , allRecords ) where

import qualified Control.Lens as L
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

data Record a = Record
  { rname :: Name a
  , rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type
  , rdata :: [String] }
  deriving (Eq, Ord)

instance Show (Record String) where
  show (Record n c ttl t d) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

data Zone a = Zone
  { root :: Name a
  , records :: [Record a]
  , children :: M.Map a (Zone a) }

insert :: Ord a => Zone a -> Record a -> Zone a
insert z@(Zone org _ _) r@(Record { rname = rn }) =
  let path = drop (length (labels org)) (NE.toList (NE.reverse (labels rn)))
  in go path z
  where
    go [] zr@(Zone _ rs _) = zr { records = r:rs  }
    go (l:path) zr@(Zone (Name zroot) _ ch) =
      let emptyZone = Zone (Name (l NE.<| zroot)) [] M.empty
          chf = if M.member l ch
            then M.update (Just . go path) l
            else M.insert l (go path emptyZone)
      in zr { children = chf ch }

allRecords :: Zone a -> [Record a]
allRecords (Zone _ rs ch) = rs ++ concatMap allRecords (M.elems ch)
