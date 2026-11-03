module NameConstraintSpec (tests) where

import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import DNS.Name (Name (Name), Named (Named))
import DNS.Record (Cat (IN), RData (RData), Type (A, MX, NS, SOA, TXT))
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile (Zone, fromList)
import NameConstraint (ConstrainedLabel, LabelConstraint (Wildcard), compileWildcards, wildcardResponse)

tests :: TestTree
tests =
  testGroup
    "NameConstraint"
    [ testCase "wildcard equality ignores constraints" $
        Wildcard ["a"] == Wildcard ["b"] @?= True
    , testCase "wildcard ordering ignores constraints" $
        compare (Wildcard ["a"]) (Wildcard ["b"]) @?= EQ
    , testGroup
        "wildcardResponse (RFC 4592 section 2.2.1)"
        [ testCase "an existing name is not synthesized" $
            wildcardAnswer ["host1", "example", ""] @?= Nothing
        , testCase "host3.example is synthesized from *.example" $
            wildcardAnswer ["host3", "example", ""] @?= Just wildcardData
        , testCase "foo.bar.example is synthesized from *.example" $
            wildcardAnswer ["foo", "bar", "example", ""] @?= Just wildcardData
        , testCase "host1.example is blocked by the existing host1 node" $
            wildcardAnswer ["host1", "example", ""] @?= Nothing
        , testCase "sub.*.example is blocked by the existing literal wildcard node" $
            wildcardAnswer ["sub", "*", "example", ""] @?= Nothing
        , testCase "_telnet._tcp.host1.example is blocked by the existing host1 subtree" $
            wildcardAnswer ["_telnet", "_tcp", "host1", "example", ""] @?= Nothing
        , testCase "host.subdel.example is blocked by the delegation" $
            wildcardAnswer ["host", "subdel", "example", ""] @?= Nothing
        , testCase "ghost.*.example is blocked by the wildcard node itself" $
            wildcardAnswer ["ghost", "*", "example", ""] @?= Nothing
        ]
    ]

-- The complete example zone from RFC 4592 section 2.2.1.  SRV is not a
-- supported record type, so its two owners are represented as empty nodes;
-- wildcardResponse only needs their existence when finding the closest encloser.
rfc4592Zone :: Zone ConstrainedLabel
rfc4592Zone = compileWildcards $ fromList $
  rr ["example", ""] (SOA nsCom hostmaster 1 2 3 4 5)
  NE.:| [ rr ["example", ""] (NS nsCom)
        , rr ["example", ""] (NS nsNet)
        , rr ["*", "example", ""] (TXT "this is a wildcard")
        , rr ["*", "example", ""] (MX 10 host1)
        , rr ["sub", "*", "example", ""] (TXT "this is not a wildcard")
        , rr ["host1", "example", ""] (A (read "192.0.2.1"))
        , node ["_ssh", "_tcp", "host1", "example", ""]
        , node ["_ssh", "_tcp", "host2", "example", ""]
        , rr ["subdel", "example", ""] (NS nsCom)
        , rr ["subdel", "example", ""] (NS nsNet)
        ]
  where
    nsCom = name ["ns", "example", "com", ""]
    nsNet = name ["ns", "example", "net", ""]
    hostmaster = name ["hostmaster", "example", ""]
    host1 = name ["host1", "example", ""]

wildcardData :: [RData]
wildcardData =
  RRSet.flattenData $
    RRSet.insert (RData (Just IN) 3600 (MX 10 (name ["host1", "example", ""]))) $
      RRSet.singleton (RData (Just IN) 3600 (TXT "this is a wildcard"))

wildcardAnswer :: [String] -> Maybe [RData]
wildcardAnswer labels = wildcardResponse (name labels) rfc4592Zone

rr :: [String] -> Type -> Named String RRSet.RDataSet
rr labels ty = Named (name labels) (RRSet.singleton (RData (Just IN) 3600 ty))

node :: [String] -> Named String RRSet.RDataSet
node labels = Named (name labels) RRSet.empty

name :: [String] -> Name String
name = Name . NE.fromList
