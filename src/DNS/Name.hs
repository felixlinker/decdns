{-# LANGUAGE FlexibleInstances #-}
module DNS.Name
  ( Name(..)
  , (<|)
  , isSubDomain
  , canonicalCompare
  , dnameRewrite ) where

import Data.List (intercalate, isSuffixOf)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower)

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

-- Order for NSEC records according to: https://datatracker.ietf.org/doc/html/rfc4034#section-6.1
canonicalCompare :: Name String -> Name String -> Ordering
canonicalCompare (Name ls1) (Name ls2) = cmp (rev ls1) (rev ls2)
  where
    rev :: NE.NonEmpty String -> [String]
    rev = map (map toLower) . reverse . NE.toList

    cmp :: Ord a => [a] -> [a] -> Ordering
    cmp [] [] = EQ
    cmp [] (_:_) = LT
    cmp (_:_) [] = GT
    cmp (h1:t1) (h2:t2)
      | h1 == h2 = cmp t1 t2
      | h1 < h2 = LT
      | otherwise = GT

dnameRewrite :: Eq a => Name a -> Name a -> Name a -> Name a
dnameRewrite qn@(Name qname) fn@(Name from) (Name to) =
  let prefixLen = length qname - length from
  in if not (isSubDomain qn fn) || prefixLen < 0
    then undefined
    else Name (NE.prependList (NE.take prefixLen qname) to)
