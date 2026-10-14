{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module DNS.Name
  ( Label(..)
  , Name(..)
  , Named(..)
  , root
  , isWildcardLabel
  , isWildcardName
  , labelCount
  , (<|)
  , (|>)
  , prependLabels
  , toStringName
  , mapName
  , mapLabels
  , isSubDomain
  , intersection
  , canonicalName
  , canonicalCompare
  , dnameRewrite ) where

import Data.List (intercalate, isSuffixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Char (toLower)
import Util.Bytes (Serialize(..), withLength)

class Label a where
  lwildcard :: a
  lempty :: a
  lshow :: a -> String

instance Label String where
  lwildcard = "*"
  lempty = ""
  lshow = id

newtype Name a = Name { labels :: NE.NonEmpty a }
  deriving (Eq, Ord)

root :: Label a => Name a
root = Name (NE.singleton lempty)

isWildcardLabel :: Label a => a -> Bool
isWildcardLabel = (lwildcard ==) . lshow

isWildcardName :: Label a => Name a -> Bool
isWildcardName = isWildcardLabel . NE.head . labels

-- See: https://datatracker.ietf.org/doc/html/rfc4034#section-3.1.3
labelCount :: (Eq a, Label a) => Name a -> Int
labelCount (Name (first NE.:| rest)) =
  length (filter (/= lempty) rest) +
    if first == lempty || first == lwildcard then 0 else 1

(<|) :: a -> Name a -> Name a
(<|) l (Name ls) = Name (l NE.<| ls)

(|>) :: Name a -> a -> Name a
(|>) (Name ls) l = Name (ls <> NE.singleton l)

prependLabels :: [a] -> Name a -> Name a
prependLabels ls (Name ls') = Name (NE.prependList ls ls')

instance Functor Name where
  fmap f (Name ls) = Name (fmap f ls)

instance Foldable Name where
  foldr f acc (Name ls) = foldr f acc ls

instance (Eq a, Label a) => Show (Name a) where

  show (Name ls) =
    let l = NE.last ls
        i = map lshow $ NE.init ls
    in  if l == lempty then intercalate "." i ++ "." else intercalate "." (i ++ [lshow l])

instance Serialize (Name String) where
  serialize = foldr ((<>) . withLength . CBS.pack) BS.empty

data Named a b = Named
  { name :: Name a
  , value :: b }
  deriving (Eq)

-- NOTE: Manual implementation of Ord so that I can rely on the invairant: if
-- name a < name b, then a < b. Used in Rollout.hs.
instance (Ord a, Ord b) => Ord (Named a b) where
  compare (Named n v) (Named n' v')
    | n /= n' = compare n n'
    | otherwise = compare v v'

instance Show a => Show (Named String a) where
  show (Named n d) = show n ++ " " ++ show d

instance Functor (Named a) where
  fmap f (Named n d) = Named n (f d)

instance Serialize a => Serialize (Named String a) where
  serialize (Named n d) = serialize n <> serialize d

toStringName :: Label a => Name a -> Name String
toStringName (Name ls) = Name $ NE.map lshow ls

mapName :: (Name a -> Name b) -> Named a v -> Named b v
mapName f (Named n v) = Named (f n) v

mapLabels :: (a1 -> a2) -> Named a1 b -> Named a2 b
mapLabels f = mapName (fmap f)

isSubDomain :: Eq a => Name a -> Name a -> Bool
isSubDomain (Name l1) (Name l2) = NE.toList l2 `isSuffixOf` NE.toList l1

intersection :: Eq a => Name a -> Name a -> Maybe (Name a)
intersection (Name (NE.reverse -> l1)) (Name (NE.reverse -> l2)) = do
    let prefix = map fst $ NE.takeWhile (uncurry (==)) $ NE.zip l1 l2
    Name <$> NE.nonEmpty (reverse prefix)

-- https://datatracker.ietf.org/doc/html/rfc4034#section-6.2
canonicalName :: Name String -> Name String
canonicalName = fmap (map toLower)

-- Order for NSEC records according to: https://datatracker.ietf.org/doc/html/rfc4034#section-6.1
canonicalCompare :: Name String -> Name String -> Ordering
canonicalCompare (canonicalName -> Name ls1) (canonicalName -> Name ls2) = cmp (rev ls1) (rev ls2)
  where
    rev :: NE.NonEmpty String -> [String]
    rev = reverse . NE.toList

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
