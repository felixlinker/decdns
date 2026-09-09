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
  , exactly
  , compileNoContext
  , compileWildcards
  , recompileWildcards
  , literalWildcards
  , overlap
  , synthesize ) where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State as St
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

import DNS.Diff
import DNS.ZoneFile
import DNS.Name
import DNS.Record
import qualified DNS.RRSet as RRSet
import Data.Maybe (fromMaybe)

import qualified Util.Tree as T
import Util.List (zipWithStrict)

data LabelConstraint a = Wildcard [a] | Exactly a
  deriving (Eq, Ord)

instance Label ConstrainedLabel where
  lwildcard = Wildcard [lwildcard]
  lempty = Exactly lempty
  lshow (Wildcard _) = "*"
  lshow (Exactly l) = lshow l

matches :: Eq a => LabelConstraint a -> LabelConstraint a -> Bool
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

compileNoContext :: Name String -> Name ConstrainedLabel
compileNoContext (Name (leftMost NE.:| rest)) = Name $ compileLabel [lwildcard] leftMost NE.:| map Exactly rest

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

type WildcardDiffs = S.Set (Diff (Named ConstrainedLabel RRSet.RDataSet))
type DiffSt = St.State WildcardDiffs

recompileWildcards :: Zone ConstrainedLabel -> (Zone ConstrainedLabel, WildcardDiffs)
recompileWildcards (Zone zOrg r) = St.runState (Zone zOrg <$> go zOrg (S.fromList $ existIn r) r) S.empty
  where
    go :: ConstrainedName -> S.Set String -> ZNode ConstrainedLabel -> DiffSt (ZNode ConstrainedLabel)
    go n@(Name (h NE.:| lblT)) newExist node@(T.Node d ch) = do
      case h of
        Wildcard (S.fromList -> oldExist) ->
          St.modify (\st -> foldr (S.insert . dataDiff) st (setDiff oldExist newExist))
        _ -> return ()
      ch' <- foldM rec M.empty (M.toList ch)
      return $ node { T.children = ch' }
      where
        rec :: ZEdges ConstrainedLabel -> (ConstrainedLabel, ZNode ConstrainedLabel) -> DiffSt (ZEdges ConstrainedLabel)
        rec acc (oldLbl, c) = do
          let exist' = existIn node
          c' <- go (oldLbl <| n) (S.fromList exist') c
          return $ M.insert (compileLabel exist' (lshow oldLbl)) c' acc

        dataDiff :: Diff String -> Diff (Named ConstrainedLabel RRSet.RDataSet)
        dataDiff = fmap (\lbl -> Named (Name $ (NE.:| lblT) $ Exactly lbl) d)

literalWildcards :: Zone ConstrainedLabel -> Zone String
literalWildcards (Zone n r) = Zone (fmap show n) (T.mapLabels show r)

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

overlap :: Eq a => Name (LabelConstraint a) -> Name (LabelConstraint a) -> Bool
overlap (Name (h1 NE.:| t1)) (Name (h2 NE.:| t2)) =
  matches h1 h2 && maybe False and (zipWithStrict (==) t1 t2)

synthesize :: Name String -> ConstrainedRecord -> StandardRecord
synthesize qname (Named cs d) = Named (fromMaybe (toStringName cs) (match qname cs)) d
