module Parsing.WellformednessSpec (tests) where

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import DNS.Name (Name, Named, (<|), labelCount)
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
    , named alias (RData IN 300 (RRSIG (RRSIGData T.CNAME RSASHA256 (labelCount alias) 300 1 0 1 org (BS.pack [1, 2, 3])))) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org

allowsGlueBelowNSZoneCut :: Assertion
allowsGlueBelowNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , named cut (RData IN 300 (NS glueName))
    , named glueName (RData IN 300 (A $ read "192.0.2.1")) ]
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
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    alias = "alias" <| cut

rejectsOtherRecordsAlongsideCNAME :: Assertion
rejectsOtherRecordsAlongsideCNAME = do
  zone <- zoneFromList
    [ soa org
    , named alias (RData IN 300 (CNAME target))
    , named alias (RData IN 300 (A $ read "192.0.2.1")) ]
  isJust (wellformed zone) @?= True
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
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org

rejectsMultipleCNAMEs :: Assertion
rejectsMultipleCNAMEs = do
  zone <- zoneFromList
    [ soa org
    , named alias (RData IN 300 (CNAME target1))
    , named alias (RData IN 300 (CNAME target2)) ]
  isJust (wellformed zone) @?= True
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
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    owner = "alias" <| org
    target1 = mkName ["one", "example", "net", ""]
    target2 = mkName ["two", "example", "net", ""]

rejectsWildcardNS :: Assertion
rejectsWildcardNS = do
  zone <- zoneFromList [soa org, named wildcard (RData IN 300 (NS ns))]
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    ns = "ns" <| org

rejectsWildcardDNAME :: Assertion
rejectsWildcardDNAME = do
  zone <- zoneFromList [soa org, named wildcard (RData IN 300 (DNAME target))]
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    target = mkName ["target", "example", "net", ""]


rejectsNonZoneDNSKEY :: Assertion
rejectsNonZoneDNSKEY = do
  zone <- zoneFromList [soa org, named org (dnskey [])]
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]

allowsAncestorSigner :: Assertion
allowsAncestorSigner = do
  zone <- zoneFromList
    [ soa org
    , named owner (RData IN 300 (A $ read "192.0.2.1"))
    , named owner (rrsig T.A (labelCount owner) org) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

rejectsSignerBelowOwner :: Assertion
rejectsSignerBelowOwner = do
  zone <- zoneFromList
    [ soa org
    , named owner (RData IN 300 (A $ read "192.0.2.1"))
    , named owner (rrsig T.A (labelCount owner) ("signer" <| owner)) ]
  isJust (wellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

requiresSOASignerAtOwner :: Assertion
requiresSOASignerAtOwner = do
  valid <- zoneFromList [soa org, named org (rrsig T.SOA orgLs org)]
  invalid <- zoneFromList [soa org, named org (rrsig T.SOA orgLs parent)]
  wellformed valid @?= Nothing
  isJust (wellformed invalid) @?= True
  where
    org = mkName ["example", "com", ""]
    orgLs = labelCount org
    parent = mkName ["com", ""]

requiresDNSKEYSignerAtOwner :: Assertion
requiresDNSKEYSignerAtOwner = do
  valid <- zoneFromList
    [ soa org, named org (dnskey [ZoneKey]), named org (rrsig T.DNSKEY orgLs org) ]
  invalid <- zoneFromList
    [ soa org, named org (dnskey [ZoneKey]), named org (rrsig T.DNSKEY orgLs owner) ]
  wellformed valid @?= Nothing
  isJust (wellformed invalid) @?= True
  where
    org = mkName ["example", "com", ""]
    orgLs = labelCount org
    owner = "key" <| org

dnskey :: [DNSKeyFlag] -> RData
dnskey flags = RData IN 300 (DNSKEY flags RSASHA256 (BS.pack [1, 2, 3]))

rrsig :: T.Type -> Int -> Name String -> RData
rrsig covered ls signer =
  RData IN 300 (RRSIG (RRSIGData covered RSASHA256 ls 300 1 0 1 signer (BS.pack [1, 2, 3])))

soa :: Name String -> Named String RRSet.RDataSet
soa org = named org (RData IN 300 (SOA ns mailbox 1 2 3 4 5))
  where
    ns = mkName ["ns", "example", "com", ""]
    mailbox = mkName ["hostmaster", "example", "com", ""]
