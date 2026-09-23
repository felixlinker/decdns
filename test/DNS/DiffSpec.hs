module DNS.DiffSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import DNS.Diff
import DNS.Name (Name, Named (..), (<|), root)
import DNS.NameSpec (mkName)
import DNS.Record
import qualified DNS.RecordType as T
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile (Zone, emptyZone, fromList, get, insert, isEmpty, set, zoneName, singleton, prune)
import NameConstraint (ConstrainedLabel, compileWildcards, compileName)

tests :: TestTree
tests =
  testGroup
    "DNS.Diff"
    [ testCase "diff reports added and deleted records at same name" reportsAddsAndDeletes
    , testCase "diff empty for unchanged records" unchangedIsEmpty
    , testCase "diff preserves non-root origin" preservesNonRootOrigin
    , testCase "hasChanges detects exact-name changes" exactChangeDetection
    , testCase "recompiling wildcards detects added names" recompileDetectsAddedNames
    , testCase "recompiling wildcards detects removed names" recompileDetectsRemovedNames
    ]

reportsAddsAndDeletes :: Assertion
reportsAddsAndDeletes = do
  let n = mkName ["www", "example", "com", ""]
      oldZ = singleton (Named n (mkData "192.0.2.1"))
      newZ = singleton (Named n (mkData "192.0.2.2"))
  let dz = diff oldZ newZ
  (M.lookup T.A . RRSet.rds =<< get n dz) @?= Just (NE.fromList [deleted (mkRData "192.0.2.1"), added (mkRData "192.0.2.2")])

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
  zoneName dz @?= compileName org
  hasChanges (compileName n) dz @?= True

exactChangeDetection :: Assertion
exactChangeDetection = do
  let n = mkName ["db", "example", "com", ""]
      oldZ = emptyZone root
      newZ = singleton (Named n (mkData "198.51.100.1"))
  let dz = diff (compileWildcards oldZ) (compileWildcards newZ)
  hasChanges (compileName n) dz @?= True
  hasChanges (compileName (mkName ["other", "example", "com", ""])) dz @?= False

recompileDetectsAddedNames :: Assertion
recompileDetectsAddedNames = do
  let addedName = "added" <| exampleCom
      (_, synthesized) = recompileWildcards $ insert wildcardZone (Named (compileName addedName) (mkData "192.0.2.1"))
  synthesized @?= S.fromList (map (Named addedName . added) (RRSet.flattenData wildcardData))

recompileDetectsRemovedNames :: Assertion
recompileDetectsRemovedNames = do
  let removedName = "fixed" <| exampleCom
      (_, synthesized) = recompileWildcards $ prune $ set (compileName removedName) RRSet.empty wildcardZone
  synthesized @?= S.fromList (map (Named removedName . deleted) (RRSet.flattenData wildcardData))

wildcardZone :: Zone ConstrainedLabel
wildcardZone = compileWildcards $ fromList
  ( Named ("*" <| exampleCom) wildcardData
  NE.:| [Named ("fixed" <| exampleCom) (mkData "192.0.2.2")]
  )

wildcardData :: RRSet.RDataSet
wildcardData = RRSet.singleton (RData IN 100 (TXT "wildcard"))

exampleCom :: Name String
exampleCom = mkName ["example", "com", ""]

mkRData :: String -> RData
mkRData = RData IN 300 . A . read

mkData :: String -> RRSet.RDataSet
mkData = RRSet.singleton . mkRData
