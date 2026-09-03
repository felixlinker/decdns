module DNS.Zone
  ( splitZone
  , collectGlue ) where

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
splitZone (Zone n r) = St.execState (rec n n r) []
  where
    rec :: Label a => Name a -> Name a -> ZNode a -> St.State [Zone a] (ZNode a)
    rec zn accN (Tr.Node d cs)
      | hasType T.SOA d = do
        let (parentData, childData) = splitData zn d
        let parentNode = Tr.Node parentData M.empty
        childCs <- M.traverseWithKey (\l -> rec accN (l <| accN)) cs
        let childNode = Tr.Node childData childCs
        St.modify (Zone accN childNode:)
        return parentNode
      | otherwise = do
        cs' <- M.traverseWithKey (\l -> rec zn (l <| accN)) cs
        return (Tr.Node d cs')

    splitData :: Label a => Name a -> RDataSet -> (RDataSet, RDataSet)
    splitData zn d@(RDS m) =
      let baseParent  = M.filterWithKey (\k _ -> k `elem` [T.DS, T.NS]) m
          baseChild   = M.delete T.DS m
          (sigsParent, sigsChild) = L.partition (`hasSignersName` toStringName zn) $ getType T.RRSIG d
          (nsecParent, nsecChild) = L.partition (not . coversSOA) $ getType T.NSEC d
          parentData  = inserts baseParent  [(T.NSEC, nsecParent),  (T.RRSIG, sigsParent)]
          childData   = inserts baseChild   [(T.NSEC, nsecChild),   (T.RRSIG, sigsChild)]
      in (RDS parentData, RDS childData)

collectGlue :: Zone a -> (Zone a, [RRSet a])
collectGlue (Zone zn r) = first (Zone zn) $ St.runState (rec zn r) []
  where
    rec :: Name a -> ZNode a -> St.State [RRSet a] (ZNode a)
    rec accn (Tr.Node d cs)
      | hasType T.NS d = do
        let glue = allRRSets (Zone accn (Tr.Node empty cs))
        St.modify (glue ++)
        return (Tr.Node d M.empty)
      | otherwise = do
        cs' <- M.traverseWithKey (\l -> rec (l <| accn)) cs
        return (Tr.Node d cs')
