{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module NameConstraint
  ( LabelConstraint(..)
  , matches
  , ConstrainedLabel
  , ConstrainedName
  , ConstrainedRecord
  , ConstrainedRRSet
  , toStandardRecord
  , exactly
  , compileWildcards
  , synthesize ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import DNS.ZoneFile
import DNS.Name
import DNS.Record
import qualified DNS.RRSet as RRSet
import Data.Maybe (fromMaybe)

import qualified Util.Tree as T

data LabelConstraint a = Wildcard [a] | Exactly a
  deriving (Eq, Ord)

instance Label a => Label (LabelConstraint a) where
  lwildcard = Wildcard [lwildcard]
  lempty = Exactly lempty
  lshow (Wildcard _) = "*"
  lshow (Exactly l) = lshow l

matches :: Ord a => LabelConstraint a -> LabelConstraint a -> Bool
matches (Exactly l1) (Exactly l2) = l1 == l2
matches (Wildcard ls) (Exactly l) = l `notElem` ls
matches (Exactly l) (Wildcard ls) = l `notElem` ls
matches (Wildcard _) (Wildcard _) = True

instance Show ConstrainedLabel where
  show (Wildcard _) = "*"
  show (Exactly l) = l

type ConstrainedLabel = LabelConstraint String
type ConstrainedName = Name ConstrainedLabel
type ConstrainedRecord = Record ConstrainedLabel
type ConstrainedRRSet = RRSet.RRSet ConstrainedLabel

toStandardRecord :: Label a => Record a -> StandardRecord
toStandardRecord (Named n d) = Named (toStringName n) d

exactly :: Name a -> Name (LabelConstraint a)
exactly (Name ls) = Name (NE.map Exactly ls)

existIn :: T.Node String v -> [String]
existIn = M.keys . T.children

compileWildcards :: Zone String -> Zone ConstrainedLabel
compileWildcards (Zone (Name n) r) = Zone (Name (NE.map Exactly n)) (go r)
  where
    mapLabel :: [String] -> String -> ConstrainedLabel
    mapLabel exist "*" = Wildcard exist
    mapLabel _ x   = Exactly x

    go :: T.Node String RRSet.RDataSet -> T.Node ConstrainedLabel RRSet.RDataSet
    go n_@(T.Node _ ch) = n_ { T.children = M.map go $ M.mapKeys (mapLabel (existIn n_)) ch }

match :: Name String -> ConstrainedName -> Maybe (Name String)
match (Name (NE.reverse -> ls)) (Name (NE.reverse -> cs)) = do
  matched <- match' ls cs
  n <- NE.nonEmpty (reverse matched)
  return $ Name n
  where
    match' :: NE.NonEmpty String -> NE.NonEmpty ConstrainedLabel -> Maybe [String]
    match' (a NE.:| as) ((Exactly c) NE.:| ncs)
      | a == c = (a:) <$> rec as ncs
      | otherwise = Nothing
    match' (a NE.:| as) ((Wildcard c) NE.:| ncs)
      | (a `notElem` c) && null ncs = Just (a:as)
      | a == "*" = (a:) <$> rec as ncs
      | otherwise = Nothing

    rec :: [String] -> [ConstrainedLabel] -> Maybe [String]
    rec [] [] = Just []
    rec _ [] = Just []
    rec [] _ = Nothing
    rec (a:as) (c:ncs) =
      match' (a NE.:| as) (c NE.:| ncs)

synthesize :: Name String -> ConstrainedRecord -> StandardRecord
synthesize qname (Named cs d) = Named (fromMaybe (toStringName cs) (match qname cs)) d
