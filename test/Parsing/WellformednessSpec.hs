module Parsing.WellformednessSpec (tests) where

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import DNS.Name (Name, Named, (<|))
import DNS.Record
import qualified DNS.RRSet as RRSet
import qualified DNS.RecordType as T
import DNS.SEC.Crypto (AlgorithmType (RSASHA256))
import DNS.ZoneFileSpec (named, zoneFromList)
import DNS.NameSpec (mkName)
import Parsing.Wellformedness (wellformed)
import Data.Maybe (isJust)

tests :: TestTree
tests =
  testGroup
    "Parsing.Wellformedness"
    [ testCase "allows DNSSEC records alongside CNAME" allowsDNSSECAlongsideCNAME
    , testCase "rejects non-DNSSEC records alongside CNAME" rejectsOtherRecordsAlongsideCNAME
    , testCase "allows glue below NS zone cut" allowsGlueBelowNSZoneCut
    , testCase "rejects CNAME below NS zone cut" rejectsCNAMEBelowNSZoneCut
    , testCase "rejects records other than delegation data at NS zone cut" rejectsOtherRecordsAtNSZoneCut
    , testCase "rejects multiple CNAME records at one owner" rejectsMultipleCNAMEs
    , testCase "rejects multiple DNAME records at one owner" rejectsMultipleDNAMEs
    , testCase "rejects NS records at wildcard leaves" rejectsWildcardNS
    , testCase "rejects DNAME records at wildcard leaves" rejectsWildcardDNAME
    , testCase "rejects DNSKEY records without the zone-key flag" rejectsNonZoneDNSKEY
    , testCase "allows an ancestor signer for ordinary RRSIG records" allowsAncestorSigner
    , testCase "rejects a signer below the owner for ordinary RRSIG records" rejectsSignerBelowOwner
    , testCase "requires an SOA RRSIG signer to equal its owner" requiresSOASignerAtOwner
    , testCase "requires a DNSKEY RRSIG signer to equal its owner" requiresDNSKEYSignerAtOwner
    ]

allowsDNSSECAlongsideCNAME :: Assertion
allowsDNSSECAlongsideCNAME = do
  zone <- zoneFromList
    [ soa org
    , named alias (RData IN 300 (CNAME (mkName ["target", "example", "net", ""])))
    , named alias (RData IN 300 (RRSIG T.CNAME RSASHA256 3 300 1 0 1 org (BS.pack [1, 2, 3]))) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org

allowsGlueBelowNSZoneCut :: Assertion
allowsGlueBelowNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , named cut (RData IN 300 (NS glueName))
    , named glueName (RData IN 300 (A "192.0.2.1")) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    glueName = "ns" <| cut

rejectsCNAMEBelowNSZoneCut :: Assertion
rejectsCNAMEBelowNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , named cut (RData IN 300 (NS ("ns" <| cut)))
    , named alias (RData IN 300 (CNAME (mkName ["target", "example", "net", ""]))) ]
  assertBool "CNAME below NS delegation should be illegal" (isJust (wellformed zone))
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    alias = "alias" <| cut

rejectsOtherRecordsAlongsideCNAME :: Assertion
rejectsOtherRecordsAlongsideCNAME = do
  zone <- zoneFromList
    [ soa org
    , named alias (RData IN 300 (CNAME target))
    , named alias (RData IN 300 (A "192.0.2.1")) ]
  wellformed zone @?= Just "CNAME record accompanied by records other than NSEC or RRSIG"
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org
    target = mkName ["target", "example", "net", ""]

rejectsOtherRecordsAtNSZoneCut :: Assertion
rejectsOtherRecordsAtNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , named cut (RData IN 300 (NS ("ns" <| cut)))
    , named cut (RData IN 300 (TXT "...")) ]
  wellformed zone @?= Just "illegal record at or below zone cut"
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org

rejectsMultipleCNAMEs :: Assertion
rejectsMultipleCNAMEs = do
  zone <- zoneFromList
    [ soa org
    , named alias (RData IN 300 (CNAME target1))
    , named alias (RData IN 300 (CNAME target2)) ]
  wellformed zone @?= Just "more than one DNAME or CNAME record at owner name"
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org
    target1 = mkName ["one", "example", "net", ""]
    target2 = mkName ["two", "example", "net", ""]

rejectsMultipleDNAMEs :: Assertion
rejectsMultipleDNAMEs = do
  zone <- zoneFromList
    [ soa org
    , named owner (RData IN 300 (DNAME target1))
    , named owner (RData IN 300 (DNAME target2)) ]
  wellformed zone @?= Just "more than one DNAME or CNAME record at owner name"
  where
    org = mkName ["example", "com", ""]
    owner = "alias" <| org
    target1 = mkName ["one", "example", "net", ""]
    target2 = mkName ["two", "example", "net", ""]

rejectsWildcardNS :: Assertion
rejectsWildcardNS = do
  zone <- zoneFromList [soa org, named wildcard (RData IN 300 (NS ns))]
  wellformed zone @?= Just "DNAME or NS wildcard record"
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    ns = "ns" <| org

rejectsWildcardDNAME :: Assertion
rejectsWildcardDNAME = do
  zone <- zoneFromList [soa org, named wildcard (RData IN 300 (DNAME target))]
  wellformed zone @?= Just "DNAME or NS wildcard record"
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    target = mkName ["target", "example", "net", ""]


rejectsNonZoneDNSKEY :: Assertion
rejectsNonZoneDNSKEY = do
  zone <- zoneFromList [soa org, named org (dnskey [])]
  wellformed zone @?= Just "DNSKEY flags miss zone key bit"
  where
    org = mkName ["example", "com", ""]

allowsAncestorSigner :: Assertion
allowsAncestorSigner = do
  zone <- zoneFromList
    [ soa org
    , named owner (RData IN 300 (A "192.0.2.1"))
    , named owner (rrsig T.A org) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

rejectsSignerBelowOwner :: Assertion
rejectsSignerBelowOwner = do
  zone <- zoneFromList
    [ soa org
    , named owner (RData IN 300 (A "192.0.2.1"))
    , named owner (rrsig T.A ("signer" <| owner)) ]
  wellformed zone @?= Just "RRSIG has illegal signer name"
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

requiresSOASignerAtOwner :: Assertion
requiresSOASignerAtOwner = do
  valid <- zoneFromList [soa org, named org (rrsig T.SOA org)]
  invalid <- zoneFromList [soa org, named org (rrsig T.SOA parent)]
  wellformed valid @?= Nothing
  wellformed invalid @?= Just "RRSIG has illegal signer name"
  where
    org = mkName ["example", "com", ""]
    parent = mkName ["com", ""]

requiresDNSKEYSignerAtOwner :: Assertion
requiresDNSKEYSignerAtOwner = do
  valid <- zoneFromList
    [ soa org, named owner (dnskey [ZoneKey]), named owner (rrsig T.DNSKEY owner) ]
  invalid <- zoneFromList
    [ soa org, named owner (dnskey [ZoneKey]), named owner (rrsig T.DNSKEY org) ]
  wellformed valid @?= Nothing
  wellformed invalid @?= Just "RRSIG has illegal signer name"
  where
    org = mkName ["example", "com", ""]
    owner = "key" <| org

dnskey :: [DNSKeyFlag] -> RData
dnskey flags = RData IN 300 (DNSKEY flags RSASHA256 (BS.pack [1, 2, 3]))

rrsig :: T.Type -> Name String -> RData
rrsig covered signer =
  RData IN 300 (RRSIG covered RSASHA256 3 300 1 0 1 signer (BS.pack [1, 2, 3]))

soa :: Name String -> Named String RRSet.RDataSet
soa org = named org (RData IN 300 (SOA ns mailbox 1 2 3 4 5))
  where
    ns = mkName ["ns", "example", "com", ""]
    mailbox = mkName ["hostmaster", "example", "com", ""]
