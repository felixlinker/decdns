{-# LANGUAGE ViewPatterns #-}
module DNS.Diff
  ( Diff(..)
  , added
  , invert
  , deleted
  , deither
  , item
  , ZoneDiff
  , RDataSetDiff
  , RRSetDiff
  , WildcardDiffs
  , diff
  , setDiff
  , hasChanges
  , patch ) where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State as St
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM
import Data.Either ( partitionEithers, isRight )
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import DNS.Name as N
import NameConstraint
import DNS.Record
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile

import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))
import Data.Bifoldable (Bifoldable(bifoldr))
import Data.Bitraversable (Bitraversable(bitraverse))
import Util.List (nonEmpty)

-- Left is deleted, Right is added
newtype Diff a = Diff
  { diffEither :: Either a a }
  deriving (Eq, Ord)

instance Functor Diff where
  fmap f (Diff e) = Diff $ bimap f f e

instance Foldable Diff where
  foldr f acc (Diff e) = bifoldr f f acc e

instance Traversable Diff where
  traverse f (Diff e) = Diff <$> bitraverse f f e

instance Show a => Show (Diff a) where
  show (Diff (Left a)) = "delete: " ++ show a
  show (Diff (Right a)) = "add: " ++ show a

deleted :: a -> Diff a
deleted = Diff . Left

added :: a  -> Diff a
added = Diff . Right

invert :: Diff a -> Diff a
invert (Diff (Left a)) = added a
invert (Diff (Right a)) = deleted a

deither :: (a -> b) -> (a -> b) -> Diff a -> b
deither l r = either l r . diffEither

item :: Diff a -> a
item (Diff (Left a)) = a
item (Diff (Right a)) = a

isAdded :: Diff a -> Bool
isAdded = isRight . diffEither

additions :: NE.NonEmpty (Diff a) -> Maybe (NE.NonEmpty a)
additions = NE.nonEmpty . map item . NE.filter isAdded

partitionDiffs :: NE.NonEmpty (Diff a) -> ([a], [a])
partitionDiffs = partitionEithers . map diffEither . NE.toList

type ZoneDiff a = AZone a RRSet.RTypedSet (Diff RData)
type RDataSetDiff = RRSet.RTypedSet (Diff RData)
type RRSetDiff = Named String RDataSetDiff

diff :: (Label a, Ord a) => Zone a -> Zone a -> ZoneDiff a
diff (fmap deleted -> Zone oldOrg oldRs) (fmap added -> Zone newOrg newRs) =
  let Path zOrg oldP newP = pathInZone oldOrg newOrg
      oldRs' = T.prepend oldP oldRs
      newRs' = T.prepend newP newRs
  in prune $ Zone zOrg $ T.mapItems (RRSet.modifyL (M.map nubEither)) $ T.merge oldRs' newRs'
  where
    nubEither :: Eq a => NE.NonEmpty (Diff a) -> [Diff a]
    nubEither es = NE.filter (deither ((`all` es) . rightNeq) ((`all` es) . leftNeq)) es

    leftNeq :: Eq a => a -> Diff a -> Bool
    leftNeq x = deither (/= x) true

    rightNeq :: Eq a => a -> Diff a -> Bool
    rightNeq x = deither true (/= x)

    true :: a -> Bool
    true = const True

setDiff :: Ord a => S.Set a -> S.Set a -> S.Set (Diff a)
setDiff oldS newS = S.mapMonotonic Diff $ S.disjointUnion (oldS `S.difference` newS) (newS `S.difference` oldS)

hasChanges :: (Label a, Ord a) => Name a -> ZoneDiff a -> Bool
hasChanges n (Zone org ns) = case pathInZone org n of
  Path _ [] p -> go p ns
  _ -> False
  where
    go :: (Label a, Ord a) => [a] -> T.Node a RDataSetDiff -> Bool
    go [] (T.Node is _) = not $ null is
    go (l : t) node = rec l t node

    rec :: (Label a, Ord a) => a -> [a] -> T.Node a RDataSetDiff -> Bool
    rec l r (T.Node _ cs) = maybe False (go r) (M.lookup l cs)

patchRData :: NE.NonEmpty (Diff RData) -> NE.NonEmpty RData -> Maybe (NE.NonEmpty RData)
patchRData diffs d = nonEmpty $ add $ delete (foldr S.insert S.empty d)
  where
    dels, adds :: [RData]
    (dels, adds) = partitionDiffs diffs

    delete :: S.Set RData -> S.Set RData
    delete s = foldr S.delete s dels

    add :: S.Set RData -> S.Set RData
    add s = foldr S.insert s adds

patchData :: RDataSetDiff -> RRSet.RDataSet -> RRSet.RDataSet
patchData (RRSet.RDS diffsM) (RRSet.RDS m) =
  RRSet.RDS $ MM.merge
    (MM.mapMaybeMissing (const additions))
    MM.preserveMissing
    (MM.zipWithMaybeMatched (\_ diffs -> patchRData diffs))
    diffsM
    m

patch :: Foldable t => Zone ConstrainedLabel -> t RRSetDiff -> (Zone ConstrainedLabel, WildcardDiffs)
patch z = recompileWildcards . foldr (\(Named (compileName -> n) d) -> adjust (aux d) n) z
  where
    aux :: RDataSetDiff -> [ConstrainedLabel] -> ZNode ConstrainedLabel -> ZNode ConstrainedLabel
    aux d [] (T.Node v cs) = T.Node (patchData d v) cs
    aux d (h:t) (T.Node v cs) =
      let adds = RRSet.map item $ RRSet.filter isAdded d
      in T.Node v $ if RRSet.null adds then cs else M.insert h (T.leafAt t adds) cs

type WildcardDiffs = S.Set RRSetDiff
type DiffSt = St.State WildcardDiffs

recompileWildcards :: Zone ConstrainedLabel -> (Zone ConstrainedLabel, WildcardDiffs)
recompileWildcards (Zone zOrg r) = St.runState (Zone zOrg <$> go zOrg (S.fromList $ existIn r) r) S.empty
  where
    go :: ConstrainedName -> S.Set String -> ZNode ConstrainedLabel -> DiffSt (ZNode ConstrainedLabel)
    go n@(Name (h NE.:| lblT)) newExist node@(T.Node d ch) = do
      case h of
        -- oldExist will be null if the wildcard has just been added; in that
        -- case, there is no diff due to changed in synthesis.
        Wildcard (S.fromList -> oldExist) | not (S.null oldExist) ->
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

        dataDiff :: Diff String -> RRSetDiff
        dataDiff diffLbl =
          Named (Name (item diffLbl NE.:| map lshow lblT)) (fmap (diffLbl $>) d)
