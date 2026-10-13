{-# LANGUAGE TupleSections #-}
module DNS.Zone
  ( splitZone
  , splitZoneAt
  , splitZones
  , collectReferrals ) where

import qualified Control.Monad.Trans.State as St
import Control.Arrow (first)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.ZoneFile
import qualified DNS.Name as N
import DNS.Record
import qualified DNS.RecordType as T
import DNS.RRSet

import qualified Util.Tree as Tr

insertNonEmpty :: Ord k => k -> [a] -> M.Map k (NE.NonEmpty a) -> M.Map k (NE.NonEmpty a)
insertNonEmpty k mv = M.alter (const $ NE.nonEmpty mv) k

inserts :: Ord k => M.Map k (NE.NonEmpty a) -> [(k, [a])] -> M.Map k (NE.NonEmpty a)
inserts = foldr (uncurry insertNonEmpty)

splitZone :: (Label a, Ord a) => Zone a -> [Zone a]
splitZone = splitZoneAt S.empty

splitZoneAt :: (Label a, Ord a) => S.Set (Name a) -> Zone a -> [Zone a]
splitZoneAt apices input =
  let Zone n r = mergeZones (emptyZone N.root) input
      (remaining, zones) = St.runState (rec n False r) []
      partial = prune (Zone n remaining)
  in if isEmpty partial then zones else partial : zones
  where
    rec accN retainParent (Tr.Node d cs)
      | hasType T.SOA d || accN `S.member` apices = do
        let (parentData, childData) = splitData accN d
        let parentNode = Tr.Node (if retainParent then parentData else empty) M.empty
        childCs <- M.traverseWithKey (\l -> rec (l <| accN) True) cs
        let childNode = Tr.Node childData childCs
        St.modify (Zone accN childNode:)
        return parentNode
      | otherwise = do
        let retainParent' = retainParent && not (hasType T.NS d)
        cs' <- M.traverseWithKey (\l -> rec (l <| accN) retainParent') cs
        return (Tr.Node d cs')

splitZones :: (Name String -> S.Set (Name String)) -> [(Name String, [StandardRecord])] -> [(Name String, Zone String)]
splitZones apices = concatMap (uncurry aux)
  where
    aux :: Name String -> [StandardRecord] -> [(Name String, Zone String)]
    aux nameServer records =
      let zones = maybe [] (splitZoneAt (apices nameServer) . fromRecordList) (NE.nonEmpty records)
      in Prelude.map (nameServer,) zones

splitData :: Label a => Name a -> RDataSet -> (RDataSet, RDataSet)
splitData childZoneName d@(RDS m) =
  let baseParent  = M.filterWithKey (\k _ -> k `elem` [T.DS, T.NS]) m
      baseChild   = M.delete T.DS m
      (sigsParent, sigsChild) = L.partition (not . (`hasSignersName` toStringName childZoneName)) $ getType T.RRSIG d
      (nsecParent, nsecChild) = L.partition (not . coversSOA) $ getType T.NSEC d
      parentData  = inserts baseParent  [(T.NSEC, nsecParent),  (T.RRSIG, sigsParent)]
      childData   = inserts baseChild   [(T.NSEC, nsecChild),   (T.RRSIG, sigsChild)]
  in (RDS parentData, RDS childData)

pickReferrals :: Zone a -> [RRSet a]
pickReferrals (Zone accn r) =
  let r' = fmap (modify (M.filterWithKey (\k _ -> k `elem` referralTypes))) r
  in allRRSets (Zone accn r')
  where
    referralTypes :: [T.Type]
    referralTypes = [T.NS, T.A, T.AAAA]

collectReferrals :: Label a => Zone a -> (Zone a, [RRSet a])
collectReferrals (Zone zn (Tr.Node apexD apexCS)) =
  first (Zone zn . Tr.Node apexD) $
    St.runState (M.traverseWithKey (\l -> rec (l <| zn)) apexCS) []
  where
    rec :: Label a => Name a -> ZNode a -> St.State [RRSet a] (ZNode a)
    rec accn (Tr.Node d cs)
      | hasType T.NS d = do
        let (parentData, childData) = splitData accn d
        let referrals = pickReferrals (Zone accn (Tr.Node childData cs))
        St.modify (referrals ++)
        return (Tr.Node (dropType T.NS parentData) M.empty)
      | otherwise = do
        cs' <- M.traverseWithKey (\l -> rec (l <| accn)) cs
        return (Tr.Node d cs')
