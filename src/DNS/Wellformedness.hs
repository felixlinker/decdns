{-# LANGUAGE ViewPatterns #-}
module DNS.Wellformedness
  ( responseWellformed
  , zoneWellformed ) where

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

(==>) :: Bool -> Bool -> Bool
(==>) p q = not p || q

-- TODO: Enforce limits on NSEC3 iterations
-- TODO: Wellformedness cannot address synthesized CNAME responses for DNAMEs;
-- or drop and check that they have been correctly synthesized
-- TODO: Enforce NSEC3 consistency according to RFC 5155, Sec. 7.2.
-- TODO: Enforce tautologies on NSEC(3) bitmaps
-- TODO: Enforce that there is the corresponding data for every RRSIG
wellformed :: Zone String -> Maybe String
wellformed z@(Zone org r) =
        -- See: https://datatracker.ietf.org/doc/html/rfc4035#section-2.4
        -- TODO: Rather ignore this
  asum  [ expect (all checkKeyFlags r) "DNSKEY flags miss zone key bit"
        , expect (checkCNAME z) "CNAME record accompanied by records other than NSEC or RRSIG"
        , expect (checkZoneCuts r) "illegal record at or below zone cut"
        , expect (singletonTypes r) "more than one CNAME, DNAME, NSEC or SOA record at owner name"
        , expect (wildcardRestrictions z) "DNAME, DS or NS wildcard record"
        , expect (rrsigSignerName org r) "RRSIG has illegal signer name" ]

responseWellformed :: Zone String -> Maybe String
responseWellformed z@(Zone _ r) =
  asum  [ wellformed z
        , expect (checkDNAME True r) "CNAME next to DNAME or records below DNAME"
        , expect (rrsigLabelCount True z) "RRSIG has illegal label count" ]

-- NOTE: I do not enforce that there must not be any DS records at the zone
-- apex, because I do not distinguish whether the data is at the parent or child
-- side of the delegation for a name server that also serves the child zone. For
-- the rule, see: https://datatracker.ietf.org/doc/html/rfc4035#section-2.4
zoneWellformed :: Zone String -> Maybe String
zoneWellformed z@(Zone _ r@(Tr.Node _ rcs)) =
  asum  [ wellformed z
        , expect (checkNoDataAboveApex r) "zone misses SOA record"
        , expect (checkDNAME False r) "CNAME next to DNAME or records below DNAME"
        , expect (rrsigLabelCount False z) "RRSIG has illegal label count"
        , expect (noDNSKEY dataNoApex) "DNSKEY with non-apex owner name" ]
  where
    dataNoApex :: Tr.Node String (RRSet.RTypedSet RData)
    dataNoApex = Tr.Node RRSet.empty rcs

getType :: T.Type -> RRSet.RDataSet -> Maybe (NE.NonEmpty RData)
getType t = M.lookup t . RRSet.rds

hasType :: T.Type -> RRSet.RDataSet -> Bool
hasType t = M.member t . RRSet.rds

typesAllIn :: RRSet.RTypedSet a -> [T.Type] -> Bool
typesAllIn rd = RRSet.apply M.null . foldr (RRSet.modify . M.delete) rd

checkKeyFlags :: RRSet.RDataSet -> Bool
checkKeyFlags = all (checkFlag . rtype)
  where
    checkFlag :: Type -> Bool
    checkFlag (DNSKEY (DNSKEYData fs _ _)) = ZoneKey `elem` fs
    checkFlag _ = True

-- See: https://datatracker.ietf.org/doc/html/rfc4034#section-3; third paragraph
-- This also covers that a CNAME must not be at zone apex (apex will have SOA record).
checkCNAME :: Zone String -> Bool
checkCNAME = allNamed (const checkData)
  where
    checkData :: RRSet.RDataSet -> Bool
    checkData rs = hasType T.CNAME rs ==> typesAllIn rs [T.CNAME, T.NSEC, T.RRSIG]

singletonTypes :: Tr.Node String RRSet.RDataSet -> Bool
singletonTypes = all (\rs -> all (`checkOne` rs) [T.CNAME, T.DNAME, T.NSEC, T.SOA])
  where
    checkOne :: T.Type -> RRSet.RDataSet -> Bool
    checkOne t = maybe True (null . NE.tail) . getType t

wildcardRestrictions :: (Label a, Eq a) => Zone a -> Bool
wildcardRestrictions = allNamed f
  where
    f :: (Label a, Eq a) => Name a -> RRSet.RDataSet -> Bool
    f (labels -> (label NE.:| _)) rs = any (`hasType` rs) [T.DNAME, T.DS, T.NS] ==> (label /= lwildcard)

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

rrsigLabelCount :: Bool -> Zone String -> Bool
rrsigLabelCount considerSynthesis = allNamed checkData
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData owner = all checkRRSIG
      where
        checkRRSIG :: RData -> Bool
        -- NOTE: >= because I may receive signatures on synthesized responses
        checkRRSIG (rtype -> RRSIG sig)
          | considerSynthesis = labelCount owner >= _labels sig
          | otherwise         = labelCount owner == _labels sig
        checkRRSIG _ = True

noDNSKEY :: Tr.Node String RRSet.RDataSet -> Bool
noDNSKEY = Tr.allNamed checkData N.root  -- Use N.root because implementation ignores name
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData _ = not . hasType T.DNSKEY

-- See: https://datatracker.ietf.org/doc/html/rfc6672#section-2.4
checkDNAME :: Bool -> Tr.Node String RRSet.RDataSet -> Bool
checkDNAME allowCNAME (Tr.Node rs cs)
  | hasType T.DNAME rs = not (hasType T.CNAME rs)
      && (M.null cs || (allowCNAME && all onlyCNAME cs))
  | otherwise = all (checkDNAME allowCNAME) cs
  where
    onlyCNAME :: Tr.Node String RRSet.RDataSet -> Bool
    onlyCNAME = all (`typesAllIn` [T.CNAME])

checkZoneCuts :: Tr.Node String RRSet.RDataSet -> Bool
checkZoneCuts (Tr.Node rs cs)
  | hasType T.NS rs && not (hasType T.SOA rs) =
      typesAllIn rs [T.NS, T.DS, T.NSEC, T.RRSIG, T.A, T.AAAA]
        && all (all (`typesAllIn` [T.A, T.AAAA])) cs
  | otherwise = all checkZoneCuts cs

checkNoDataAboveApex :: Tr.Node String RRSet.RDataSet -> Bool
checkNoDataAboveApex (Tr.Node rs cs)
  | RRSet.null rs = all checkNoDataAboveApex cs
  | otherwise = hasType T.SOA rs
