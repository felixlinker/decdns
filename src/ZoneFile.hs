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

newtype Name a = Name { labels :: [a] }
  deriving (Eq, Ord)

instance Functor Name where
  fmap f (Name ls) = Name (fmap f ls)

instance Show (Name String) where
  show (Name ls) = intercalate "." ls

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

instance Show (Record String) where
  show (Record n c ttl t d) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " " ++ unwords d

data Zone = Zone
  { root :: Name String
  , records :: [Record String]
  , children :: M.Map String Zone }

insert :: Zone -> Record String -> Zone
insert z_@(Zone org _ _) r_ = go (labels org) r_ z_
  where
    go :: [String] -> Record String -> Zone -> Zone
    go acc r z@(Zone _ rs ch) = case L.unsnoc (labels (rname r)) of
      Nothing -> Zone org (r { rname = Name acc }:rs) ch
      Just (ls, l) ->
        let rec = go (l:acc) (r { rname = Name ls })
            ch' = if M.member l ch
              then M.update (Just . rec) l ch
              else M.insert l (rec (Zone (Name [l]) [] M.empty)) ch
        in z { children = ch' }

allRecords :: Zone -> [Record String]
allRecords (Zone _ rs ch) = rs ++ concatMap allRecords (M.elems ch)
