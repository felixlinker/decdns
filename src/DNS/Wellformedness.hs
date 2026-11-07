{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module DNS.Wellformedness
  ( Issue(..)
  , isFatal
  , responseWellformed
  , zoneWellformed ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
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

data Issue a = Warning a | Error a
  deriving (Eq, Ord)

instance Show a => Show (Issue a) where
  show (Warning a) = "Warning: " ++ show a
  show (Error a) = "Error: " ++ show a

isFatal :: Issue a -> Bool
isFatal (Error _) = True
isFatal (Warning _) = False

expect :: Bool -> Issue String -> Maybe (Issue String)
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
wellformed :: Zone String -> [Issue String]
wellformed z@(Zone org r@(Tr.Node rd _)) = catMaybes
  [ expect (checkCNAME z) (Error "CNAME record accompanied by records other than NSEC or RRSIG")
  , expect (not $ RRSet.hasType T.DS rd) (Error "DS record at zone apex")
  , expect (not $ any (RRSet.hasType T.SOA) (dataNoApex z)) (Error "zone SOA record below apex")
  , expect (checkZoneCuts $ dataNoApex z) (Error "illegal record at or below zone cut")
  , expect (singletonTypes r) (Error "more than one CNAME, DNAME, NSEC or SOA record at owner name")
  , expect (wildcardRestrictions z) (Error "DNAME, DS or NS wildcard record")
  , expect (rrsigSignerName org r) (Error "RRSIG has illegal signer name")
  , expect (checkTTL r) (Warning "RRSet containing records with different TTLs")
  , expect (checkTTLRRsig r) (Warning "RRSIG with different TTL than covered RRSet") ]

responseWellformed :: Zone String -> [Issue String]
responseWellformed z@(Zone _ r) = wellformed z ++ catMaybes
  [ expect (checkDNAME True r) (Error "CNAME next to DNAME or records below DNAME")
  , expect (rrsigLabelCount True z) (Error "RRSIG has illegal label count") ]

-- NOTE: I do not enforce that there must not be any DS records at the zone
-- apex, because I do not distinguish whether the data is at the parent or child
-- side of the delegation for a name server that also serves the child zone. For
-- the rule, see: https://datatracker.ietf.org/doc/html/rfc4035#section-2.4
zoneWellformed :: Zone String -> [Issue String]
zoneWellformed z@(Zone _ r@(Tr.Node rd _)) = wellformed z ++ catMaybes
  [ expect (RRSet.hasType T.SOA rd) (Error "zone misses SOA record")
  , expect (RRSet.hasType T.NS rd) (Error "zone misses apex NS records")
  , expect (checkDNAME False r) (Error "CNAME next to DNAME or records below DNAME")
  , expect (rrsigLabelCount False z) (Error "RRSIG has illegal label count")
  , expect (noDNSSECKey (dataNoApex z)) (Error "DNSKEY with non-apex owner name") ]

typesAllIn :: RRSet.RTypedSet a -> [T.Type] -> Bool
typesAllIn rd = M.null . RRSet.rds . foldr (RRSet.modify . M.delete) rd

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
      all ((\case { DNSKEY k -> not (isDNSSECKey k); _ -> True}) . rtype) keys

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

-- https://www.rfc-editor.org/info/rfc2181/#section-5.2
-- https://www.rfc-editor.org/info/rfc4034/#section-3
checkTTL :: Tr.Node String RRSet.RDataSet -> Bool
checkTTL = all (all (\(r0 NE.:| rt) -> all ((== rttl r0) . rttl) rt) . RRSet.rds . RRSet.dropType T.RRSIG)

checkTTLRRsig :: Tr.Node String RRSet.RDataSet -> Bool
checkTTLRRsig = all check
  where
    check :: RRSet.RDataSet -> Bool
    check rs = all checkCovered (RRSet.getType T.RRSIG rs)
      where
        checkCovered :: RData -> Bool
        checkCovered (RData _ ttl (RRSIG (RRSIGData { _typeCovered = cov }))) =
          all ((== ttl) . rttl) (RRSet.getType cov rs)
        checkCovered _ = True -- technically undefined
