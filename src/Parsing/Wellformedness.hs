module Parsing.Wellformedness
  ( wellformed ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.ZoneFile
import qualified DNS.RRSet as RRSet
import qualified DNS.RecordType as T
import qualified Util.Tree as Tr

data ZoneCut = CNAMECut | NSCut

getZoneCut :: RRSet.RDataSet -> Maybe ZoneCut
getZoneCut rs
  | hasType T.CNAME rs = Just CNAMECut
  | hasType T.NS rs = Just NSCut
  | otherwise = Nothing

wellformed :: Zone String -> Maybe String
wellformed (Zone org r@(Tr.Node apexData rcs)) =
  asum  [ guard wildcardOrigin $> "zone origin has wildcard"
        , guard noSOA $> "zone misses SOA record"
        , guard (M.member T.CNAME (RRSet.rds apexData)) $> "no CNAME at APEX"
        , guard (checkZoneCuts org (Tr.Node (RRSet.modify (M.delete T.NS) apexData) rcs)) $> "illegal record at or below zone cut"
        , guard (wildcardRestrictions orgLabel r) $> "DNAME or NS wildcard record" ]
  where
    orgLabel :: String
    orgLabel = NE.head $ labels org

    wildcardOrigin :: Bool
    wildcardOrigin = "*" == orgLabel

    noSOA :: Bool
    noSOA = M.notMember T.SOA (RRSet.rds apexData)

    checkZoneCuts :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
    checkZoneCuts n (Tr.Node rs cs) = maybe rec recCheck (getZoneCut rs)
      where
        rec :: Bool
        rec = or $ M.mapWithKey (\k -> checkZoneCuts (k <| n)) cs

        recCheck :: ZoneCut -> Bool
        recCheck cut = not (typesAllowedAtCut cut rs)
            || not (all (typesAllowedBelowCut cut) (Tr.Node RRSet.empty cs))

hasType :: T.Type -> RRSet.RDataSet -> Bool
hasType t = M.member t . RRSet.rds

typesAllIn :: RRSet.RTypedSet a -> [T.Type] -> Bool
typesAllIn rd = RRSet.apply M.null . foldr (RRSet.modify . M.delete) rd

typesAllowedAtCut :: ZoneCut -> RRSet.RDataSet -> Bool
typesAllowedAtCut cut rd = typesAllIn rd (cutRecord cut : [T.DS, T.NSEC, T.RRSIG])

cutRecord :: ZoneCut -> T.Type
cutRecord CNAMECut = T.CNAME
cutRecord NSCut = T.NS

typesAllowedBelowCut :: ZoneCut -> RRSet.RDataSet -> Bool
typesAllowedBelowCut CNAMECut rd = RRSet.apply M.null rd
typesAllowedBelowCut NSCut rd = typesAllIn rd [T.A, T.AAAA]

wildcardRestrictions :: String -> Tr.Node String RRSet.RDataSet -> Bool
wildcardRestrictions label (Tr.Node rs cs)
  | label == "*" && M.null cs = any (`M.member` RRSet.rds rs) [T.DNAME, T.NS]
  | otherwise = any (uncurry wildcardRestrictions) (M.toList cs)
