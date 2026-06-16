module DNS.DiffSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Diff
import DNS.Name (Named (..), (<|), root)
import DNS.NameSpec (mkName)
import DNS.Record
import qualified DNS.RecordType as T
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile (emptyZone, get, insert, isEmpty, zoneName, singleton)
import DNS.ZoneFileSpec (zoneFromList)
import NameConstraint (LabelConstraint (..), compileWildcards, exactly)

tests :: TestTree
tests =
  testGroup
    "DNS.Diff"
    [ testCase "diff reports added and deleted records at same name" reportsAddsAndDeletes
    , testCase "diff empty for unchanged records" unchangedIsEmpty
    , testCase "diff preserves non-root origin" preservesNonRootOrigin
    , testCase "hasChanges detects exact-name changes" exactChangeDetection
    , testCase "hasChanges wildcard respects excluded labels" wildcardChangeDetection
    ]

reportsAddsAndDeletes :: Assertion
reportsAddsAndDeletes = do
  let n = mkName ["www", "example", "com", ""]
      oldZ = singleton (Named n (mkData "192.0.2.1"))
      newZ = singleton (Named n (mkData "192.0.2.2"))
  let dz = diff oldZ newZ
  (RRSet.apply (M.lookup T.A) =<< get n dz) @?= Just (NE.fromList [deleted (mkRData "192.0.2.1"), added (mkRData "192.0.2.2")])

unchangedIsEmpty :: Assertion
unchangedIsEmpty = do
  let n = mkName ["api", "example", "com", ""]
      z = singleton (Named n (mkData "192.0.2.10"))
      dz = diff z z
  isEmpty dz @?= True
  get n dz @?= Just RRSet.empty

preservesNonRootOrigin :: Assertion
preservesNonRootOrigin = do
  let org = mkName ["example", "com", ""]
      n = "www" <| org
      oldZ = insert (emptyZone org) (Named n (mkData "192.0.2.1"))
      newZ = insert (emptyZone org) (Named n (mkData "192.0.2.2"))
      dz = diff (compileWildcards oldZ) (compileWildcards newZ)
  zoneName dz @?= exactly org
  hasChanges (exactly n) dz @?= True

exactChangeDetection :: Assertion
exactChangeDetection = do
  let n = mkName ["db", "example", "com", ""]
      oldZ = emptyZone root
      newZ = singleton (Named n (mkData "198.51.100.1"))
  let dz = diff (compileWildcards oldZ) (compileWildcards newZ)
  hasChanges (exactly n) dz @?= True
  hasChanges (exactly (mkName ["other", "example", "com", ""])) dz @?= False

wildcardChangeDetection :: Assertion
wildcardChangeDetection = do
  let foo = mkName ["foo", "example", "com", ""]
      bar = mkName ["bar", "example", "com", ""]
      oldZ = emptyZone root
  newZ <- zoneFromList [Named foo (mkData "203.0.113.1"), Named bar (mkData "203.0.113.2")]
  let dz = diff (compileWildcards oldZ) (compileWildcards newZ)
      wildcard = Wildcard ["foo"] <| exactly (mkName ["example", "com", ""])
  hasChanges wildcard dz @?= True

mkRData :: String -> RData
mkRData = RData IN 300 . A

mkData :: String -> RRSet.RDataSet
mkData = RRSet.singleton . mkRData
