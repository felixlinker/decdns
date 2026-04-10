module DNS.DiffSpec (tests) where

import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)

import DNS.Diff
import DNS.Name (Name (..), Named (..), Label, (<|), root)
import DNS.Record
import DNS.ZoneFile (AZone, emptyZone, fromList, get, insert, isEmpty, zoneName, singleton)
import NameConstraint (LabelConstraint (..), exactly)

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
  let n = mkName ["www", "example", "com"]
      oldZ = singleton (Named n (mkData "192.0.2.1"))
      newZ = singleton (Named n (mkData "192.0.2.2"))
  let dz = diff oldZ newZ
  (get n dz == Just [deleted (mkData "192.0.2.1"), added (mkData "192.0.2.2")]) @?= True

unchangedIsEmpty :: Assertion
unchangedIsEmpty = do
  let n = mkName ["api", "example", "com"]
      z = singleton (Named n (mkData "192.0.2.10"))
      dz = diff z z
  isEmpty dz @?= True
  get n dz @?= Just []

preservesNonRootOrigin :: Assertion
preservesNonRootOrigin = do
  let org = mkName ["example", "com"]
      n = "www" <| org
      oldZ = insert (emptyZone org) (Named n (mkData "192.0.2.1"))
      newZ = insert (emptyZone org) (Named n (mkData "192.0.2.2"))
      dz = diff oldZ newZ
  zoneName dz @?= org
  hasChanges (exactly n) dz @?= True

exactChangeDetection :: Assertion
exactChangeDetection = do
  let n = mkName ["db", "example", "com"]
      oldZ = emptyZone root
      newZ = singleton (Named n (mkData "198.51.100.1"))
  let dz = diff oldZ newZ
  hasChanges (exactly n) dz @?= True
  hasChanges (exactly (mkName ["other", "example", "com"])) dz @?= False

wildcardChangeDetection :: Assertion
wildcardChangeDetection = do
  let foo = mkName ["foo", "example", "com"]
      bar = mkName ["bar", "example", "com"]
      oldZ = emptyZone root
  newZ <- zoneFromList [Named foo (mkData "203.0.113.1"), Named bar (mkData "203.0.113.2")]
  let dz = diff oldZ newZ
      wildcard = Wildcard ["foo"] <| exactly (mkName ["example", "com"])
  hasChanges wildcard dz @?= True

mkData :: String -> RData
mkData ip = RData IN 300 (A ip)

mkName :: [String] -> Name String
mkName [] = error "mkName: empty list"
mkName xs = Name (NE.fromList (xs ++ [""]))

zoneFromList :: (Label a, Ord a) => [Named a b] -> IO (AZone a b)
zoneFromList = maybe (fail "could not create zone") return . fromList
