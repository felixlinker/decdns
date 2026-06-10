{-# LANGUAGE ViewPatterns #-}
module Parsing.Wellformedness
  ( wellformed ) where

import Control.Applicative (asum)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.ZoneFile
import qualified DNS.RRSet as RRSet
import DNS.Record
import qualified DNS.RecordType as T
import qualified Util.Tree as Tr

expect :: Bool -> String -> Maybe String
expect p err = if p then Nothing else Just err

expectNot :: Bool -> String -> Maybe String
expectNot p err = if p then Just err else Nothing

(==>) :: Bool -> Bool -> Bool
(==>) p q = not p || q

wellformed :: Zone String -> Maybe String
wellformed (Zone org r@(Tr.Node apexData rcs)) =
  asum  [ expectNot wildcardOrigin "zone origin has wildcard"
        , expect hasSOA "zone misses SOA record"
        , expect dnskeyFlags "DNSKEY flags miss zone key bit"
        , expect (checkCNAME org r) "CNAME record accompanied by records other than NSEC or RRSIG"
        -- Ignore records at apex. Other records next to NSes are legal there.
        , expect (checkZoneCuts org (Tr.Node RRSet.empty rcs)) "illegal record at or below zone cut"
        , expect (singletonTypes r) "more than one DNAME or CNAME record at owner name"
        , expect (wildcardRestrictions orgLabel r) "DNAME or NS wildcard record"
        , expect (rrsigSignerName org r) "RRSIG has illegal signer name" ]
  where
    orgLabel :: String
    orgLabel = NE.head $ labels org

    wildcardOrigin :: Bool
    wildcardOrigin = "*" == orgLabel

    hasSOA :: Bool
    hasSOA = M.member T.SOA (RRSet.rds apexData)

    dnskeyFlags :: Bool
    dnskeyFlags = all (all (checkFlag . rtype)) r
      where
        checkFlag :: Type -> Bool
        checkFlag (DNSKEY fs _ _) = ZoneKey `elem` fs
        checkFlag _ = True

    -- See: https://datatracker.ietf.org/doc/html/rfc4034#section-3; third paragraph
    -- This also covers that a CNAME must not be at zone apex (apex will have SOA record).
    checkCNAME :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
    checkCNAME = Tr.allNamed (const checkData)
      where
        checkData :: RRSet.RDataSet -> Bool
        checkData rs = hasType T.CNAME rs ==> typesAllIn rs [T.CNAME, T.NSEC, T.RRSIG]

    -- TODO: Can have records below CNAME
    checkZoneCuts :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
    checkZoneCuts n (Tr.Node rs cs) = if hasType T.NS rs
        then typesAllIn rs [T.NS, T.DS, T.NSEC, T.RRSIG, T.A, T.AAAA] && all (`typesAllIn` [T.A, T.AAAA]) (Tr.Node RRSet.empty cs)
        else and $ M.mapWithKey (\k -> checkZoneCuts (k <| n)) cs

    singletonTypes :: Tr.Node String RRSet.RDataSet -> Bool
    singletonTypes = all (\rs -> checkOne T.CNAME rs && checkOne T.DNAME rs)
      where
        checkOne :: T.Type -> RRSet.RDataSet -> Bool
        checkOne t = maybe True (null . NE.tail) . getType t

getType :: T.Type -> RRSet.RDataSet -> Maybe (NE.NonEmpty RData)
getType t = M.lookup t . RRSet.rds

hasType :: T.Type -> RRSet.RDataSet -> Bool
hasType t = M.member t . RRSet.rds

typesAllIn :: RRSet.RTypedSet a -> [T.Type] -> Bool
typesAllIn rd = RRSet.apply M.null . foldr (RRSet.modify . M.delete) rd

wildcardRestrictions :: String -> Tr.Node String RRSet.RDataSet -> Bool
wildcardRestrictions label (Tr.Node rs cs)
  | label == "*" && M.null cs = all (`M.notMember` RRSet.rds rs) [T.DNAME, T.NS]
  | otherwise = all (uncurry wildcardRestrictions) (M.toList cs)

rrsigSignerName :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
rrsigSignerName = Tr.allNamed checkData
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData owner rs = all checkRRSIG $ maybe [] NE.toList (getType T.RRSIG rs)
      where
        checkRRSIG :: RData -> Bool
        checkRRSIG (rtype -> sig@(RRSIG {})) =
          let signer = _signersName sig
          in case _typeCovered sig of
            T.SOA -> signer == owner
            T.DNSKEY -> signer == owner
            _ -> isSubDomain owner signer
        checkRRSIG _ = True
