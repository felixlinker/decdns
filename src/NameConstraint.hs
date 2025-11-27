{-# LANGUAGE ViewPatterns #-}
module NameConstraint
  ( NameConstraint(..)
  , compileWildcards
  , match ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import ZoneFile

data NameConstraint a = Wildcard [a] | Exactly a
  deriving (Eq, Ord)

existIn :: Node String -> [String]
existIn = filter (/= "*") . M.keys . children

compileWildcards :: Zone String -> Zone (NameConstraint String)
compileWildcards (Zone (Name n) r) = Zone (Name (NE.map Exactly n)) (go r)
  where
    mapLabel :: [String] -> String -> NameConstraint String
    mapLabel exist "*" = Wildcard exist
    mapLabel _ x   = Exactly x

    go :: Node String -> Node (NameConstraint String)
    go n_@(Node _ ch) = n_ { children = M.map go $ M.mapKeys (mapLabel (existIn n_)) ch }

match :: Name String -> Name (NameConstraint String) -> Maybe (Name String)
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
