{-# LANGUAGE ViewPatterns #-}
module Parsing.Wellformedness
  ( wellformed ) where

import Control.Applicative (asum)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name as N
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

-- TODO: Enforce limits on NSEC3 iterations
-- TODO: Wellformedness cannot address synthesized CNAME responses for DNAMEs;
-- or drop and check that they have been correctly synthesized
-- TODO: Enforce NSEC3 consistency according to RFC 5155, Sec. 7.2.
-- TODO: Enforce tautologies on NSEC(3) bitmaps
-- TODO: Enforce that there is the corresponding data for every RRSIG
wellformed :: Name String -> Zone String -> Maybe String
wellformed ns (Zone org r@(Tr.Node apexData rcs)) =
  asum  [ expectNot wildcardOrigin "zone origin has wildcard"
        , expect (hasType T.SOA apexData) "zone misses SOA record"
        , expectNot (any (hasType T.SOA) dataNoApex) "zone has non-apex SOA"
        -- See: https://datatracker.ietf.org/doc/html/rfc4035#section-2.4
        , expectNot (hasType T.DS apexData) "DS record at zone apex"
        -- TODO: Rather ignore this
        , expect dnskeyFlags "DNSKEY flags miss zone key bit"
        , expect (checkCNAME org r) "CNAME record accompanied by records other than NSEC or RRSIG"
        -- TODO: Below check is too restrictive. A name server serving this zone
        -- may be the delegated name server, in which case there may be records
        -- at or below the NS record.
        -- Ignore records at apex. Other records next to NSes are legal there.
        , expect (checkZoneCuts org dataNoApex) "illegal record at or below zone cut"
        , expect (checkDNAME r) "CNAME next to DNAME or records below DNAME"
        -- TODO: Address as limitation
        , expectNot (any (selfDelegation ns) dataNoApex) "NS record is self-delegation"
        -- TODO: Add NSEC, SOA as singleton types (maybe also NSEC3?)
        , expect (singletonTypes r) "more than one DNAME or CNAME record at owner name"
        , expect (wildcardRestrictions orgLabel r) "DNAME, DS or NS wildcard record"
        , expect (rrsigSignerName org r) "RRSIG has illegal signer name"
        , expect (rrsigLabelCount org r) "RRSIG has illegal label count"
        , expect (noDNSKEY dataNoApex) "DNSKEY with non-apex owner name" ]
  where
    dataNoApex :: Tr.Node String (RRSet.RTypedSet RData)
    dataNoApex = Tr.Node RRSet.empty rcs

    orgLabel :: String
    orgLabel = NE.head $ labels org

    wildcardOrigin :: Bool
    wildcardOrigin = "*" == orgLabel

    dnskeyFlags :: Bool
    dnskeyFlags = all (all (checkFlag . rtype)) r
      where
        checkFlag :: Type -> Bool
        checkFlag (DNSKEY (DNSKEYData fs _ _)) = ZoneKey `elem` fs
        checkFlag _ = True

    -- See: https://datatracker.ietf.org/doc/html/rfc4034#section-3; third paragraph
    -- This also covers that a CNAME must not be at zone apex (apex will have SOA record).
    checkCNAME :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
    checkCNAME = Tr.allNamed (const checkData)
      where
        checkData :: RRSet.RDataSet -> Bool
        checkData rs = hasType T.CNAME rs ==> typesAllIn rs [T.CNAME, T.NSEC, T.RRSIG]

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
  | label == "*" && M.null cs = all (`M.notMember` RRSet.rds rs) [T.DNAME, T.DS, T.NS]
  | otherwise = all (uncurry wildcardRestrictions) (M.toList cs)

rrsigSignerName :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
rrsigSignerName = Tr.allNamed checkData
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData owner rs = all checkRRSIG $ maybe [] NE.toList (getType T.RRSIG rs)
      where
        checkRRSIG :: RData -> Bool
        checkRRSIG (rtype -> RRSIG sig) =
          let signer = _signersName sig
          in case _typeCovered sig of
            T.SOA -> signer == owner
            T.DNSKEY -> signer == owner
            T.DS -> signer /= owner && isSubDomain owner signer
            _ -> isSubDomain owner signer
        checkRRSIG _ = True

rrsigLabelCount :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
rrsigLabelCount = Tr.allNamed checkData
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData owner = all checkRRSIG
      where
        checkRRSIG :: RData -> Bool
        -- NOTE: >= because I may receive signatures on synthesized responses
        checkRRSIG (rtype -> RRSIG sig) = labelCount owner >= _labels sig
        checkRRSIG _ = True

noDNSKEY :: Tr.Node String RRSet.RDataSet -> Bool
noDNSKEY = Tr.allNamed checkData N.root  -- Use N.root because implementation ignores name
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData _ = not . hasType T.DNSKEY

selfDelegation :: Name String -> RRSet.RDataSet -> Bool
selfDelegation ns = maybe False (any ((== NS ns) . rtype)) . getType T.NS

-- See: https://datatracker.ietf.org/doc/html/rfc6672#section-2.4
checkDNAME :: Tr.Node String RRSet.RDataSet -> Bool
checkDNAME (Tr.Node rs cs)
  | hasType T.DNAME rs = not (hasType T.CNAME rs) && M.null cs
  | otherwise = all checkDNAME cs
