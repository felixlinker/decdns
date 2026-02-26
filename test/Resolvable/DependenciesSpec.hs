module Resolvable.DependenciesSpec (tests) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

import DNS.Name (Name (Name), Named (Named))
import DNS.Record (Cat (IN), RData (RData), Type (A, NS), record)
import NameConstraint (exactly)
import Resolvable.Dependencies (Dependencies (deps), ResolvableAt (ResolvableAt), fromRules)
import Resolvable.Resolvability (compileConfig)
import Util.Fresh (runFresh)

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
      delegatedResolution = ResolvableAt (Named delegatedNs (exactly delegatedName))

  case M.lookup rootDelegation (deps dependencies) of
    Nothing -> assertFailure "expected root NS delegation record to have a dependency entry"
    Just ds ->
      assertBool
        "expected dependency on delegated nameserver resolving a name in the delegated domain"
        (delegatedResolution `S.member` ds)

mkName :: [String] -> Name String
mkName = Name . NE.fromList
