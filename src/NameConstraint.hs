{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NameConstraint
  ( LabelConstraint(..)
  , ConstrainedLabel
  , ConstrainedName
  , ConstrainedRecord
  , ConstrainedRRSet
  , toStandardRecord
  , compileName
  , compileLabel
  , existIn
  , compileWildcards
  , literalWildcards
  , synthesize
  , wildcardResponse ) where

import Control.Monad (guard)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import DNS.ZoneFile
import DNS.Name
import DNS.Record
import qualified DNS.RRSet as RRSet

import qualified Util.Tree as T
import Control.Applicative (asum)

data LabelConstraint a = Wildcard [a] | Exactly a

instance Eq a => Eq (LabelConstraint a) where
  Wildcard _ == Wildcard _ = True
  Exactly l1 == Exactly l2 = l1 == l2
  Wildcard _ == Exactly _ = False
  Exactly _ == Wildcard _ = False

instance Ord a => Ord (LabelConstraint a) where
  compare (Wildcard _) (Wildcard _) = EQ
  compare (Wildcard _) (Exactly _) = LT
  compare (Exactly _) (Wildcard _) = GT
  compare (Exactly l1) (Exactly l2) = compare l1 l2

instance Label ConstrainedLabel where
  lwildcard = Wildcard []
  lempty = Exactly lempty
  lshow (Wildcard _) = "*"
  lshow (Exactly l) = lshow l

instance Show ConstrainedLabel where
  show (Wildcard _) = "*"
  show (Exactly l) = l

type ConstrainedLabel = LabelConstraint String
type ConstrainedName = Name ConstrainedLabel
type ConstrainedRecord = Record ConstrainedLabel
type ConstrainedRRSet = RRSet.RRSet ConstrainedLabel

toStandardRecord :: Label a => Record a -> StandardRecord
toStandardRecord = mapName toStringName

compileName :: Name String -> ConstrainedName
compileName = fmap (compileLabel [])

compileLabel :: [String] -> String -> ConstrainedLabel
compileLabel exist lbl
  | isWildcardLabel lbl = Wildcard exist
  | otherwise = Exactly lbl

existIn :: Label a => T.Node a v -> [String]
existIn = map lshow . M.keys . T.children

compileWildcards :: Zone String -> Zone ConstrainedLabel
compileWildcards (Zone (Name n) r) = Zone (Name (NE.map Exactly n)) (go r)
  where
    go :: T.Node String RRSet.RDataSet -> ZNode ConstrainedLabel
    go n_@(T.Node _ ch) = n_ { T.children = M.map go $ M.mapKeys (compileLabel (existIn n_)) ch }

literalWildcards :: Zone ConstrainedLabel -> Zone String
literalWildcards (Zone n r) = Zone (fmap show n) (T.mapLabels show r)

synthesize :: Name String -> ConstrainedName -> Bool
synthesize (Name (reverse . NE.toList -> ls)) (Name (reverse . NE.toList -> cs)) =
  match' ls cs
  where
    match' :: [String] -> [ConstrainedLabel] -> Bool
    match' [] [] = True
    match' (a:as) (Exactly c:ncs) = a == c && match' as ncs
    match' (a:_) [Wildcard c] = a `notElem` c
    match' ("*":as) (Wildcard _:ncs) = match' as ncs
    match' _ _ = False

wildcardResponse :: Name String -> Zone ConstrainedLabel -> Maybe [RData]
wildcardResponse n (Zone org r) = case pathInZone org (compileName n) of
  Path _ [] p -> walk org p r
  _ -> Nothing
  where
    walk :: Name ConstrainedLabel -> [ConstrainedLabel] -> T.Node ConstrainedLabel (RRSet.RTypedSet RData) -> Maybe [RData]
    -- Case 1: We found the data, but without synthesis.
    walk (Name (l NE.:| _)) [] (T.Node d _) = case l of
      Exactly _   -> Nothing
      Wildcard _  -> Just $ RRSet.flattenData d
    -- Case 2: If there is no child, we may need to perform synthesis.
    walk acc (h:t) (T.Node _ cs) = asum
      [ do -- Case 1: We find an exact match
        c <- M.lookup h cs
        walk (h <| acc) t c
      , do -- Case 2: Check whether synthesis is possible
        T.Node d _ <- M.lookup lwildcard cs
        guard (synthesize n (Wildcard (map lshow $ M.keys cs) <| acc))
        return $ RRSet.flattenData d ]
