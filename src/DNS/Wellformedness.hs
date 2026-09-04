{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List (uncons)
import DNS.SEC.Verification (isDNSSECKey)
import Util.Bool ((==>))

expect :: Bool -> String -> Maybe String
expect p err = if p then Nothing else Just err

dataNoApex :: Zone a -> Tr.Node a (RRSet.RTypedSet RData)
dataNoApex (Zone _ (Tr.Node _ rcs)) = Tr.Node RRSet.empty rcs

-- NOTE: Following well-formedness checks where not implemented yet, but could
-- be implemented. I believe I do not need them for my tool to work, but they
-- may be useful feedback nonetheless.
-- * Limits on NSEC3 iterations
-- * NSEC3 consistency according to RFC 5155, Sec. 7.2.
-- * Enfore tautologies on NSEC(3) bitmaps
-- * There must be the corresponding data for every RRSIG
wellformed :: Zone String -> Maybe String
wellformed z@(Zone org r) =
  asum  [ expect (checkCNAME z) "CNAME record accompanied by records other than NSEC or RRSIG"
        , expect (not $ any (RRSet.hasType T.SOA) (dataNoApex z)) "zone SOA record below apex"
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
zoneWellformed z@(Zone _ r@(Tr.Node rd _)) =
  asum  [ wellformed z
        , expect (RRSet.hasType T.SOA rd) "zone misses SOA record"
        , expect (checkDNAME False r) "CNAME next to DNAME or records below DNAME"
        , expect (rrsigLabelCount False z) "RRSIG has illegal label count"
        , expect (noDNSSECKey (dataNoApex z)) "DNSKEY with non-apex owner name" ]

typesAllIn :: RRSet.RTypedSet a -> [T.Type] -> Bool
typesAllIn rd = RRSet.apply M.null . foldr (RRSet.modify . M.delete) rd

-- See: https://datatracker.ietf.org/doc/html/rfc4034#section-3; third paragraph
-- This also covers that a CNAME must not be at zone apex (apex will have SOA record).
checkCNAME :: Zone String -> Bool
checkCNAME = allNamed (const checkData)
  where
    checkData :: RRSet.RDataSet -> Bool
    checkData rs = RRSet.hasType T.CNAME rs ==> typesAllIn rs [T.CNAME, T.NSEC, T.RRSIG]

singletonTypes :: Tr.Node String RRSet.RDataSet -> Bool
singletonTypes = all (\rs -> all (`checkOne` rs) [T.CNAME, T.DNAME, T.NSEC, T.SOA])
  where
    checkOne :: T.Type -> RRSet.RDataSet -> Bool
    checkOne t = (maybe True (null . snd) . uncons) . RRSet.getType t

wildcardRestrictions :: (Label a, Eq a) => Zone a -> Bool
wildcardRestrictions = allNamed f
  where
    f :: (Label a, Eq a) => Name a -> RRSet.RDataSet -> Bool
    f (labels -> (label NE.:| _)) rs = any (`RRSet.hasType` rs) [T.DNAME, T.DS, T.NS] ==> (label /= lwildcard)

rrsigSignerName :: Name String -> Tr.Node String RRSet.RDataSet -> Bool
rrsigSignerName = Tr.allNamed checkData
  where
    checkData :: Name String -> RRSet.RDataSet -> Bool
    checkData owner rs = all checkRRSIG (RRSet.getType T.RRSIG rs)
      where
        checkRRSIG :: RData -> Bool
        checkRRSIG (rtype -> RRSIG sig) =
          let signer = _signersName sig
          in case _typeCovered sig of
            T.SOA -> signer == owner
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

noDNSSECKey :: Tr.Node String RRSet.RDataSet -> Bool
noDNSSECKey = all checkData
  where
    checkData :: RRSet.RDataSet -> Bool
    checkData (RRSet.getType T.DNSKEY -> keys) =
      all ((\case { DNSKEY k -> isDNSSECKey k; _ -> True}) . rtype) keys

-- See: https://datatracker.ietf.org/doc/html/rfc6672#section-2.4
checkDNAME :: Bool -> Tr.Node String RRSet.RDataSet -> Bool
checkDNAME allowCNAME (Tr.Node rs cs)
  | RRSet.hasType T.DNAME rs = not (RRSet.hasType T.CNAME rs)
      && (M.null cs || (allowCNAME && all onlyCNAME cs))
  | otherwise = all (checkDNAME allowCNAME) cs
  where
    onlyCNAME :: Tr.Node String RRSet.RDataSet -> Bool
    onlyCNAME = all (`typesAllIn` [T.CNAME])

checkZoneCuts :: Tr.Node String RRSet.RDataSet -> Bool
checkZoneCuts (Tr.Node rs cs)
  | RRSet.hasType T.NS rs =
      typesAllIn rs [T.NS, T.DS, T.NSEC, T.RRSIG, T.A, T.AAAA]
        && all (all (`typesAllIn` [T.A, T.AAAA])) cs
  | otherwise = all checkZoneCuts cs
