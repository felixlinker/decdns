module DNS.WellformednessSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import DNS.Name (Name, Named(..), (<|), labelCount)
import DNS.Record
import qualified DNS.RRSet as RRSet
import qualified DNS.RecordType as T
import DNS.SEC.Crypto (AlgorithmType (RSASHA256))
import DNS.ZoneFile (fromRecordList)
import DNS.ZoneFileSpec (named, zoneFromList)
import DNS.NameSpec (mkName)
import DNS.Wellformedness (Issue (Error), isFatal, zoneWellformed)
import Parsing.ZoneFile (parseRecords)

tests :: TestTree
tests =
  testGroup
    "DNS.Wellformedness"
    [ testCase "allows DNSSEC records alongside CNAME" allowsDNSSECAlongsideCNAME
    , testCase "rejects non-DNSSEC records alongside CNAME" rejectsOtherRecordsAlongsideCNAME
    , testCase "allows glue below NS zone cut" allowsGlueBelowNSZoneCut
    , testCase "rejects CNAME below NS zone cut" rejectsCNAMEBelowNSZoneCut
    , testCase "rejects records other than delegation data at NS zone cut" rejectsOtherRecordsAtNSZoneCut
    , testCase "rejects multiple CNAME records at one owner" rejectsMultipleCNAMEs
    , testCase "rejects multiple DNAME records at one owner" rejectsMultipleDNAMEs
    , testCase "rejects NS records at wildcard leaves" rejectsWildcardNS
    , testCase "rejects DNAME records at wildcard leaves" rejectsWildcardDNAME
    , testCase "allows an ancestor signer for ordinary RRSIG records" allowsAncestorSigner
    , testCase "rejects a signer below the owner for ordinary RRSIG records" rejectsSignerBelowOwner
    , testCase "requires an SOA RRSIG signer to equal its owner" requiresSOASignerAtOwner
    , testCase "requires a DNSKEY RRSIG signer to equal its owner" requiresDNSKEYSignerAtOwner
    , testCase "rejects a zone key below the apex" rejectNonApexDNSKEY
    , testGroup "reports issues in zone files" (map reportsWellformednessIssues malformedZoneFiles)
    ]

malformedZoneFiles :: [FilePath]
malformedZoneFiles =
  [ "examples/wellformedness/ns-wc"
  ]

reportsWellformednessIssues :: FilePath -> TestTree
reportsWellformednessIssues path = testCase path $ do
  contents <- readFile path
  records <- either (assertFailure . show) return =<< parseRecords path contents
  zone <- maybe (assertFailure "zone file contains no records") (return . fromRecordList) (NE.nonEmpty records)
  not (null $ zoneWellformed zone) @?= True

allowsDNSSECAlongsideCNAME :: Assertion
allowsDNSSECAlongsideCNAME = do
  zone <- zoneFromList
    [ apex org
    , named alias (RData (Just IN) 300 (CNAME (mkName ["target", "example", "net", ""])))
    , named alias (RData (Just IN) 300 (RRSIG (RRSIGData T.CNAME RSASHA256 (labelCount alias) 300 1 0 1 org (BS.pack [1, 2, 3])))) ]
  zoneWellformed zone @?= []
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org

allowsGlueBelowNSZoneCut :: Assertion
allowsGlueBelowNSZoneCut = do
  zone <- zoneFromList
    [ apex org
    , named cut (RData (Just IN) 300 (NS glueName))
    , named glueName (RData (Just IN) 300 (A $ read "192.0.2.1")) ]
  zoneWellformed zone @?= []
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    glueName = "ns" <| cut

rejectsCNAMEBelowNSZoneCut :: Assertion
rejectsCNAMEBelowNSZoneCut = do
  zone <- zoneFromList
    [ apex org
    , named cut (RData (Just IN) 300 (NS ("ns" <| cut)))
    , named alias (RData (Just IN) 300 (CNAME (mkName ["target", "example", "net", ""]))) ]
  zoneWellformed zone @?= [Error "illegal record at or below zone cut"]
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    alias = "alias" <| cut

rejectsOtherRecordsAlongsideCNAME :: Assertion
rejectsOtherRecordsAlongsideCNAME = do
  zone <- zoneFromList
    [ apex org
    , named alias (RData (Just IN) 300 (CNAME target))
    , named alias (RData (Just IN) 300 (A $ read "192.0.2.1")) ]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org
    target = mkName ["target", "example", "net", ""]

rejectsOtherRecordsAtNSZoneCut :: Assertion
rejectsOtherRecordsAtNSZoneCut = do
  zone <- zoneFromList
    [ apex org
    , named cut (RData (Just IN) 300 (NS ("ns" <| cut)))
    , named cut (RData (Just IN) 300 (TXT "...")) ]
  zoneWellformed zone @?= [Error "illegal record at or below zone cut"]
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org

rejectsMultipleCNAMEs :: Assertion
rejectsMultipleCNAMEs = do
  zone <- zoneFromList
    [ apex org
    , named alias (RData (Just IN) 300 (CNAME target1))
    , named alias (RData (Just IN) 300 (CNAME target2)) ]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org
    target1 = mkName ["one", "example", "net", ""]
    target2 = mkName ["two", "example", "net", ""]

rejectsMultipleDNAMEs :: Assertion
rejectsMultipleDNAMEs = do
  zone <- zoneFromList
    [ apex org
    , named owner (RData (Just IN) 300 (DNAME target1))
    , named owner (RData (Just IN) 300 (DNAME target2)) ]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    owner = "alias" <| org
    target1 = mkName ["one", "example", "net", ""]
    target2 = mkName ["two", "example", "net", ""]

rejectsWildcardNS :: Assertion
rejectsWildcardNS = do
  zone <- zoneFromList [apex org, named wildcard (RData (Just IN) 300 (NS ns))]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    ns = "ns" <| org

rejectsWildcardDNAME :: Assertion
rejectsWildcardDNAME = do
  zone <- zoneFromList [apex org, named wildcard (RData (Just IN) 300 (DNAME target))]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    wildcard = "*" <| org
    target = mkName ["target", "example", "net", ""]

allowsAncestorSigner :: Assertion
allowsAncestorSigner = do
  zone <- zoneFromList
    [ apex org
    , named owner (RData (Just IN) 300 (A $ read "192.0.2.1"))
    , named owner (rrsig T.A (labelCount owner) org) ]
  zoneWellformed zone @?= []
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

rejectsSignerBelowOwner :: Assertion
rejectsSignerBelowOwner = do
  zone <- zoneFromList
    [ apex org
    , named owner (RData (Just IN) 300 (A $ read "192.0.2.1"))
    , named owner (rrsig T.A (labelCount owner) ("signer" <| owner)) ]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    owner = "www" <| org

requiresSOASignerAtOwner :: Assertion
requiresSOASignerAtOwner = do
  valid <- zoneFromList [apex org, named org (rrsig T.SOA orgLs org)]
  invalid <- zoneFromList [apex org, named org (rrsig T.SOA orgLs parent)]
  zoneWellformed valid @?= []
  any isFatal (zoneWellformed invalid) @?= True
  where
    org = mkName ["example", "com", ""]
    orgLs = labelCount org
    parent = mkName ["com", ""]

requiresDNSKEYSignerAtOwner :: Assertion
requiresDNSKEYSignerAtOwner = do
  valid <- zoneFromList
    [ apex org, named org (dnskey [ZoneKey]), named org (rrsig T.DNSKEY orgLs org) ]
  invalid <- zoneFromList
    [ apex org, named org (dnskey [ZoneKey]), named org (rrsig T.DNSKEY orgLs owner) ]
  zoneWellformed valid @?= []
  any isFatal (zoneWellformed invalid) @?= True
  where
    org = mkName ["example", "com", ""]
    orgLs = labelCount org
    owner = "key" <| org

rejectNonApexDNSKEY :: Assertion
rejectNonApexDNSKEY = do
  zone <- zoneFromList [apex org, named owner $ dnskey [ZoneKey]]
  any isFatal (zoneWellformed zone) @?= True
  where
    org = mkName ["example", "com", ""]
    owner = "key" <| org

dnskey :: [DNSKeyFlag] -> RData
dnskey flags = RData (Just IN) 300 (DNSKEY (DNSKEYData flags RSASHA256 (BS.pack [1, 2, 3])))

rrsig :: T.Type -> Int -> Name String -> RData
rrsig covered ls signer =
  RData (Just IN) 300 (RRSIG (RRSIGData covered RSASHA256 ls 300 1 0 1 signer (BS.pack [1, 2, 3])))

apex :: Name String -> Named String RRSet.RDataSet
apex org = Named org
  $ RRSet.insert (RData (Just IN) 300 (NS ns))
  $ RRSet.singleton (RData (Just IN) 300 (SOA ns mailbox 1 2 3 4 5))
  where
    ns = mkName ["ns", "example", "com", ""]
    mailbox = mkName ["hostmaster", "example", "com", ""]
