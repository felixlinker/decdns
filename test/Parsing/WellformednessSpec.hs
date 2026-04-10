module Parsing.WellformednessSpec (tests) where

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import DNS.Name (Name, Named (Named), (<|))
import DNS.Record
import qualified DNS.RecordType as T
import DNS.SEC.Crypto (AlgorithmType (RSASHA256))
import DNS.ZoneFileSpec (zoneFromList)
import DNS.NameSpec (mkName)
import Parsing.Wellformedness (wellformed)
import Data.Maybe (isJust)

tests :: TestTree
tests =
  testGroup
    "Parsing.Wellformedness"
    [ testCase "allows DNSSEC records alongside CNAME" allowsDNSSECAlongsideCNAME
    , testCase "allows glue below NS zone cut" allowsGlueBelowNSZoneCut
    , testCase "rejects CNAME below NS zone cut" rejectsCNAMEBelowNSZoneCut
    , testCase "rejects records below CNAME cut" rejectsRecordsBelowCNAMECut
    ]

allowsDNSSECAlongsideCNAME :: Assertion
allowsDNSSECAlongsideCNAME = do
  zone <- zoneFromList
    [ soa org
    , Named alias (RData IN 300 (CNAME (mkName ["target", "example", "net", ""])))
    , Named alias (RData IN 300 (RRSIG T.CNAME RSASHA256 3 300 1 0 1 org (BS.pack [1, 2, 3]))) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org

allowsGlueBelowNSZoneCut :: Assertion
allowsGlueBelowNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , Named cut (RData IN 300 (NS glueName))
    , Named glueName (RData IN 300 (A "192.0.2.1")) ]
  wellformed zone @?= Nothing
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    glueName = "ns" <| cut

rejectsCNAMEBelowNSZoneCut :: Assertion
rejectsCNAMEBelowNSZoneCut = do
  zone <- zoneFromList
    [ soa org
    , Named cut (RData IN 300 (NS ("ns" <| cut)))
    , Named alias (RData IN 300 (CNAME (mkName ["target", "example", "net", ""]))) ]
  assertBool "CNAME below NS delegation should be illegal" (isJust (wellformed zone))
  where
    org = mkName ["example", "com", ""]
    cut = "child" <| org
    alias = "alias" <| cut

rejectsRecordsBelowCNAMECut :: Assertion
rejectsRecordsBelowCNAMECut = do
  zone <- zoneFromList
    [ soa org
    , Named alias (RData IN 300 (CNAME (mkName ["target", "example", "net", ""])))
    , Named ("child" <| alias) (RData IN 300 (A "192.0.2.2")) ]
  assertBool "records below CNAME cut should be illegal" (isJust (wellformed zone))
  where
    org = mkName ["example", "com", ""]
    alias = "alias" <| org

soa :: Name String -> Named String RData
soa org =
  Named org
    (RData IN 300 (SOA ns mailbox 1 2 3 4 5))
  where
    ns = mkName ["ns", "example", "com", ""]
    mailbox = mkName ["hostmaster", "example", "com", ""]
