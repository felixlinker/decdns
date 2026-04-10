module Resolvable.DependenciesSpec (tests) where

import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

import DNS.Name (Named (Named))
import DNS.NameSpec (mkName)
import DNS.Record (Cat (IN), RData (RData), Type (A, NS), record)
import NameConstraint (exactly)
import Resolvable.Dependencies (ResolvableAt (ResolvableAt), fromRules)
import Resolvable.Resolvability (compileConfig)
import Util.Fresh (runFresh)
import qualified Util.PartialOrder as PO

tests :: TestTree
tests =
  testGroup
    "Resolvable.Dependencies"
    [ testCase
        "root delegation depends on delegated nameserver resolving names in delegated domain"
        rootDelegationDependsOnDelegatedResolution
    ]

rootDelegationDependsOnDelegatedResolution :: Assertion
rootDelegationDependsOnDelegatedResolution = do
  let rootNs = mkName ["a", "root", ""]
      delegatedNs = mkName ["ns", "example", "com", ""]
      delegatedDomain = mkName ["example", "com", ""]
      delegatedName = mkName ["www", "example", "com", ""]
      ttl = 300

      rootRecords =
        [ record (exactly delegatedDomain) (RData IN ttl (NS delegatedNs))
        , record (exactly delegatedNs) (RData IN ttl (A "192.0.2.1"))
        ]

      delegatedRecords =
        [ record (exactly delegatedName) (RData IN ttl (A "198.51.100.10"))
        ]

      compiled =
        runFresh $
          compileConfig [rootNs]
            [ (rootNs, rootRecords)
            , (delegatedNs, delegatedRecords)
            ]

      dependencies = fromRules compiled
      rootDelegation = ResolvableAt (Named rootNs (exactly delegatedDomain))
      delegatedResolution = ResolvableAt (Named delegatedNs (exactly delegatedDomain))
      delegationDeps = PO.rel rootDelegation dependencies

  assertBool
    "expected dependency on delegated nameserver resolving the delegated name"
    (delegatedResolution `S.member` delegationDeps)
