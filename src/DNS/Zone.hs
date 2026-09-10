module DNS.Zone
  ( splitZone
  , collectReferrals ) where

import qualified Control.Monad.Trans.State as St
import Control.Arrow (first)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.ZoneFile
import DNS.Record
import qualified DNS.RecordType as T
import DNS.RRSet

import qualified Util.Tree as Tr

type ZNode a = Tr.Node a RDataSet

insertNonEmpty :: Ord k => k -> [a] -> M.Map k (NE.NonEmpty a) -> M.Map k (NE.NonEmpty a)
insertNonEmpty k mv = M.alter (const $ NE.nonEmpty mv) k

inserts :: Ord k => M.Map k (NE.NonEmpty a) -> [(k, [a])] -> M.Map k (NE.NonEmpty a)
inserts = foldr (uncurry insertNonEmpty)

splitZone :: Label a => Zone a -> [Zone a]
splitZone (Zone n r) = St.execState (rec n n True r) []
  where
    rec :: Label a => Name a -> Name a -> Bool -> ZNode a -> St.State [Zone a] (ZNode a)
    rec zn accN retainParent (Tr.Node d cs)
      | hasType T.SOA d = do
        let (parentData, childData) = splitData zn d
        let parentNode = Tr.Node (if retainParent then parentData else empty) M.empty
        childCs <- M.traverseWithKey (\l -> rec accN (l <| accN) True) cs
        let childNode = Tr.Node childData childCs
        St.modify (Zone accN childNode:)
        return parentNode
      | otherwise = do
        let retainParent' = retainParent && not (hasType T.NS d)
        cs' <- M.traverseWithKey (\l -> rec zn (l <| accN) retainParent') cs
        return (Tr.Node d cs')

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
