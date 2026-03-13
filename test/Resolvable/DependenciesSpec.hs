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
import Resolvable.Prolog (Rule (Rule), Term (..))
import Resolvable.Resolvability (RComp (RComp), compileConfig)
import Util.Fresh (runFresh)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Dependencies"
    [ testCase
        "root delegation depends on delegated nameserver resolving names in delegated domain"
        rootDelegationDependsOnDelegatedResolution
    , testCase
        "merges dependencies for multiple records with same name at same nameserver"
        mergesDependenciesForSameNameAtSameNameserver
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

mergesDependenciesForSameNameAtSameNameserver :: Assertion
mergesDependenciesForSameNameAtSameNameserver = do
  let ns = mkName ["a", "root", ""]
      owner = mkName ["www", "example", "com", ""]
      depName1 = mkName ["ns1", "example", "com", ""]
      depName2 = mkName ["ns2", "example", "com", ""]
      ttl = 300

      ann name ip = record (exactly name) (RData IN ttl (A ip))
      comp name ip = RComp ns (ann name ip)
      rterm tag = TPred "resolves" [TAtom tag, TAtom tag, TAtom tag, TAtom tag]

      -- Two records at the same owner name and nameserver collapse to the same dependency key.
      ownerRule1 = Rule (comp owner "192.0.2.1") (rterm "owner1") [rterm "dep1"]
      ownerRule2 = Rule (comp owner "192.0.2.2") (rterm "owner2") [rterm "dep2"]
      depRule1 = Rule (comp depName1 "198.51.100.1") (rterm "dep1") []
      depRule2 = Rule (comp depName2 "198.51.100.2") (rterm "dep2") []

      dependencies = fromRules [ownerRule1, ownerRule2, depRule1, depRule2]
      ownerKey = ResolvableAt (Named ns (exactly owner))
      expected =
        S.fromList
          [ ResolvableAt (Named ns (exactly depName1))
          , ResolvableAt (Named ns (exactly depName2))
          ]

  case M.lookup ownerKey (deps dependencies) of
    Nothing -> assertFailure "expected merged dependency entry for owner name at nameserver"
    Just ds ->
      if ds == expected
        then pure ()
        else assertFailure ("unexpected dependencies: " ++ show ds)

mkName :: [String] -> Name String
mkName = Name . NE.fromList
