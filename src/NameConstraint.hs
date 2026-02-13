{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module NameConstraint
  ( NameConstraint(..)
  , ConstrainedName
  , ConstrainedRecord
  , toName
  , exactly
  , compileWildcards
  , synthesize ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import DNS.ZoneFile
import DNS.Name
import DNS.Record
import Data.Maybe (fromMaybe)

import qualified Util.Tree as T

-- TODO: Refactor to LabelConstraint
data NameConstraint a = Wildcard [a] | Exactly a
  deriving (Eq, Ord)

instance Label a => Label (NameConstraint a) where
  lempty = Exactly lempty

instance Show (NameConstraint String) where
  show (Wildcard _) = "*"
  show (Exactly l) = l

type ConstrainedName = Name (NameConstraint String)
type ConstrainedRecord = Record ConstrainedName

instance Show ConstrainedName where
  show = show . toName

exactly :: Name String -> ConstrainedName
exactly (Name ls) = Name (NE.map Exactly ls)

toName :: ConstrainedName -> Name String
toName (Name cs) = Name (NE.map show cs)

existIn :: T.Node String v -> [String]
existIn = filter (/= "*") . M.keys . T.children

compileWildcards :: AZone f String -> AZone f (NameConstraint String)
compileWildcards (Zone (Name n) r) = Zone (Name (NE.map Exactly n)) (go r)
  where
    mapLabel :: [String] -> String -> NameConstraint String
    mapLabel exist "*" = Wildcard exist
    mapLabel _ x   = Exactly x

    go :: T.Node String (f RData) -> T.Node (NameConstraint String) (f RData)
    go n_@(T.Node _ ch) = n_ { T.children = M.map go $ M.mapKeys (mapLabel (existIn n_)) ch }

match :: Name String -> ConstrainedName -> Maybe (Name String)
match (Name (NE.reverse -> ls)) (Name (NE.reverse -> cs)) = do
  matched <- match' ls cs
  n <- NE.nonEmpty (reverse matched)
  return $ Name n
  where
    match' :: NE.NonEmpty String -> NE.NonEmpty (NameConstraint String) -> Maybe [String]
    match' (a NE.:| as) ((Exactly c) NE.:| ncs)
      | a == c = (a:) <$> rec as ncs
      | otherwise = Nothing
    match' (a NE.:| as) ((Wildcard c) NE.:| ncs)
      | (a /= "*") && (a `notElem` c) && null ncs = Just (a:as)
      | a == "*" = (a:) <$> rec as ncs
      | otherwise = Nothing

    rec :: [String] -> [NameConstraint String] -> Maybe [String]
    rec [] [] = Just []
    rec _ [] = Just []
    rec [] _ = Nothing
    rec (a:as) (c:ncs) =
      match' (a NE.:| as) (c NE.:| ncs)

synthesize :: Name String -> ConstrainedRecord -> StandardRecord
synthesize qname (Record cs d) = Record (fromMaybe (toName cs) (match qname cs)) d
