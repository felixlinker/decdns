module Integration (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S

import DNS.Diff (hasChanges)
import DNS.Name (Name, (<|))
import DNS.NameSpec (mkName)
import DNS.Record (StandardRecord)
import qualified DNS.RecordType as RT
import DNS.ZoneFile (Zone, allRecords, fromRecordList, isEmpty, zoneName)
import NameConstraint
  ( ConstrainedLabel
  , LabelConstraint (Wildcard)
  , compileWildcards
  , exactly
  )
import Parsing (parseName)
import Parsing.ZoneFile (parseRecords)
import Parsing.Wellformedness (wellformed)
import Resolvable.Dependencies (ConstrainedQuery (CQuery), computeResolvable, queryFromRule)
import Resolvable.Prolog.Core (Term (TWildcard), ad, nameTerm, resolves, top, ttypeTerm)
import Resolvable.Resolvability (NameServersConfig, compileConfig, plResolvable)
import Resolvable.Update.Dependencies (computeRegressions)
import Resolvable.Update.Rollout (NSDiffs, diffServers)
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Util.Fresh (runFresh)

tests :: TestTree
tests =
  testGroup
    "Examples.CName"
    [ testGroup "Parsing/DNS.ZoneFile/Wellformedness" $
        map exampleFileTest exampleFiles
    , testGroup "Resolvable.Resolvability" $
        map exampleConfigTest exampleConfigs
    , testGroup
        "Resolvable.Resolvability authentication"
        [ testCase "signed authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest "signed" signedFiles
        , testCase "signed-nsec3 authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest "signed-nsec3" signedNsec3Files
        , testCase "partially-signed authenticates unsigned child zone through NSEC" $
            authenticatedUnsignedDelegationTest "partially-signed" partiallySignedFiles
        , testCase "partially-signed-nsec3 authenticates unsigned child zone through NSEC3 owner proof" $
            authenticatedUnsignedDelegationTest "partially-signed-nsec3" partiallySignedNsec3Files
        , testCase "partially-signed-nsec3-oo filters invalid NSEC3 unsigned-delegation proof" $
            doesNotAuthenticateUnsignedDelegationTest "partially-signed-nsec3-oo" partiallySignedNsec3OOFiles
        , testCase "signed does not authenticate the child zone as unsigned through NSEC" $
            signedDelegationDoesNotUseUnsignedProofTest "signed" signedFiles
        , testCase "signed-nsec3 does not authenticate the child zone as unsigned through NSEC3" $
            signedDelegationDoesNotUseUnsignedProofTest "signed-nsec3" signedNsec3Files
        , testCase "base does not authenticate unsigned answers" $ do
            results <- resolvableQueries baseFiles
            assertResolutions "base" results []
              [ authenticated nsCom RT.A aCom
              , authenticated nsCom RT.A bCom
              , authenticated nsCom RT.CNAME aCom
              , authenticated nsCom RT.NS bCom
              , authenticated nsBCom RT.A bCom ] ]
    , testGroup
        "Resolvable.Update.Dependencies"
        [ testCase "base to new has expected unsigned regression facts" $
            regressionTest "new" newFiles
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.SOA com ]
        , testCase "base to broken has expected unsigned regression facts" $
            regressionTest "broken" brokenFiles
              [ Query nsCom RT.A aCom
              , Query nsCom RT.A bCom
              , Query nsCom RT.A nsBCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.SOA com ] ]
    , testGroup
        "DNS.Diff/Resolvable.Update.Rollout.diffServers"
        [ testCase "base to new has expected record-level changes" $
            diffTest "new" newFiles
              [ Changed nsCom (exactly aCom)
              , Changed nsCom (Wildcard [] <| exactly com)
              , ServerChanged nsBCom ]
              [ Unchanged nsCom (exactly net) ]
        , testCase "base to broken has expected record-level changes" $
            diffTest "broken" brokenFiles
              [ Changed nsCom (exactly bCom)
              , Changed nsCom (exactly nsBCom)
              , ServerChanged nsBCom ]
              [ Unchanged nsCom (exactly aCom)
              , Unchanged nsCom (exactly net) ] ] ]

data ExampleFile = ExampleFile
  { examplePath :: FilePath
  , expectedOrigin :: Name String
  , expectedRecordCount :: Int
  }

exampleFiles :: [ExampleFile]
exampleFiles =
  [ ExampleFile "examples/cname/base/ns.com." com 5
  , ExampleFile "examples/cname/base/ns.b.com." bCom 4
  , ExampleFile "examples/cname/new/ns.com." com 3
  , ExampleFile "examples/cname/broken/ns.com." com 3
  , ExampleFile "examples/cname/partially-signed/ns.com." com 18
  , ExampleFile "examples/cname/partially-signed/ns.b.com." bCom 4
  , ExampleFile "examples/cname/signed/ns.com." com 20
  , ExampleFile "examples/cname/signed/ns.b.com." bCom 16
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.com." com 20
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.b.com." bCom 4
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.com." com 18
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.b.com." bCom 4
  , ExampleFile "examples/cname/signed-nsec3/ns.com." com 22
  , ExampleFile "examples/cname/signed-nsec3/ns.b.com." bCom 18 ]

data ExampleConfig = ExampleConfig
  { configName :: String
  , configFiles :: [FilePath]
  , expectedQueries :: [Query]
  , unexpectedQueries :: [Query]
  }

exampleConfigs :: [ExampleConfig]
exampleConfigs =
  [ ExampleConfig
      "base"
      baseFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.NS aCom
      , Query nsCom RT.NS bCom
      , Query nsCom RT.SOA aCom
      , Query nsCom RT.SOA bCom
      , Query nsBCom RT.A bCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "new"
      newFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.A nsBCom
      , Query nsCom RT.SOA com ]
      [ Query nsCom RT.CNAME aCom
      , Query nsCom RT.NS bCom ]
  , ExampleConfig
      "broken"
      brokenFiles
      [ Query nsCom RT.CNAME aCom
      , Query nsCom RT.SOA com ]
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.NS bCom ]
  , ExampleConfig
      "partially-signed"
      partiallySignedFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC aCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "signed"
      signedFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.DNSKEY bCom
      , Query nsCom RT.DS bCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3"
      partiallySignedNsec3Files
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3-oo"
      partiallySignedNsec3OOFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "signed-nsec3"
      signedNsec3Files
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.DNSKEY bCom
      , Query nsCom RT.DS bCom
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom] ]

baseFiles, newFiles, brokenFiles, partiallySignedFiles, signedFiles :: [FilePath]
partiallySignedNsec3Files, partiallySignedNsec3OOFiles, signedNsec3Files :: [FilePath]
baseFiles =
  [ "examples/cname/base/ns.com."
  , "examples/cname/base/ns.b.com." ]

newFiles = ["examples/cname/new/ns.com."]

brokenFiles = ["examples/cname/broken/ns.com."]

partiallySignedFiles =
  [ "examples/cname/partially-signed/ns.com."
  , "examples/cname/partially-signed/ns.b.com." ]

signedFiles =
  [ "examples/cname/signed/ns.com."
  , "examples/cname/signed/ns.b.com." ]

partiallySignedNsec3Files =
  [ "examples/cname/partially-signed-nsec3/ns.com."
  , "examples/cname/partially-signed-nsec3/ns.b.com." ]

partiallySignedNsec3OOFiles =
  [ "examples/cname/partially-signed-nsec3-oo/ns.com."
  , "examples/cname/partially-signed-nsec3-oo/ns.b.com." ]

signedNsec3Files =
  [ "examples/cname/signed-nsec3/ns.com."
  , "examples/cname/signed-nsec3/ns.b.com." ]

data Query = Query (Name String) RT.Type (Name String)
  deriving Eq

instance Show Query where
  show (Query ns rtype qname) =
    show ns ++ " " ++ show rtype ++ " " ++ show qname

data Resolution = Resolution Query Term Term
  deriving Eq

instance Show Resolution where
  show (Resolution query verification auth) =
    show query ++ " verified=" ++ show verification ++ " auth=" ++ show auth

authenticated :: Name String -> RT.Type -> Name String -> Resolution
authenticated ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 1)

authenticatedUnsigned :: Name String -> RT.Type -> Name String -> Resolution
authenticatedUnsigned ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 0)

com, net, aCom, bCom, nsCom, nsBCom :: Name String
com = mkName ["com", ""]
net = mkName ["net", ""]
aCom = mkName ["a", "com", ""]
bCom = mkName ["b", "com", ""]
nsCom = mkName ["ns", "com", ""]
nsBCom = mkName ["ns", "b", "com", ""]

rootServers :: [Name String]
rootServers = [nsCom]

exampleFileTest :: ExampleFile -> TestTree
exampleFileTest example =
  testCase (examplePath example) $ do
    (ns, records, zones) <- loadExampleFile (examplePath example)
    length records @?= expectedRecordCount example
    map zoneName zones @?= [expectedOrigin example]
    forM_ zones $ \zone ->
      assertWellformed (examplePath example) ns zone
    length (concatMap allRecords zones) @?= expectedRecordCount example

exampleConfigTest :: ExampleConfig -> TestTree
exampleConfigTest example =
  testCase (configName example) $ do
    results <- resolvableQueries (configFiles example)
    assertQueries
      (configName example)
      results
      (expectedQueries example)
      (unexpectedQueries example)

authenticatedResolutionTest :: String -> [FilePath] -> Assertion
authenticatedResolutionTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticated nsCom RT.A aCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.CNAME aCom
    , authenticated nsCom RT.DNSKEY com
    , authenticated nsCom RT.DNSKEY bCom
    , authenticated nsCom RT.DS bCom ]
    []

authenticatedUnsignedDelegationTest :: String -> [FilePath] -> Assertion
authenticatedUnsignedDelegationTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

signedDelegationDoesNotUseUnsignedProofTest :: String -> [FilePath] -> Assertion
signedDelegationDoesNotUseUnsignedProofTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]

doesNotAuthenticateUnsignedDelegationTest :: String -> [FilePath] -> Assertion
doesNotAuthenticateUnsignedDelegationTest label files = do
  results <- resolvableQueries files
  assertResolutions label results []
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

regressionTest :: String -> [FilePath] -> [Query] -> [Query] -> Assertion
regressionTest label newConfig expected unexpected = do
  old <- loadConfig baseFiles
  new <- loadConfig newConfig
  regressions <- assertRight $
    runFresh $
      runExceptT $
        computeRegressions
          (plResolvable =<< compileConfig rootServers old)
          (plResolvable =<< compileConfig rootServers new)
  assertQueries ("base -> " ++ label) (S.fromList regressions) expected unexpected

data DiffExpectation
  = Changed (Name String) (Name ConstrainedLabel)
  | Unchanged (Name String) (Name ConstrainedLabel)
  | ServerChanged (Name String)

diffTest :: String -> [FilePath] -> [DiffExpectation] -> [DiffExpectation] -> Assertion
diffTest _label newConfig expected unexpected = do
  old <- loadConfig baseFiles
  new <- loadConfig newConfig
  let diffs = diffServers old new
  forM_ expected (assertDiffExpectation diffs)
  forM_ unexpected (assertDiffExpectation diffs)

assertDiffExpectation :: NSDiffs -> DiffExpectation -> Assertion
assertDiffExpectation diffs expectation =
  case expectation of
    Changed ns name ->
      assertBool
        ("expected record changes on " ++ show ns ++ " at " ++ show name)
        (hasChange ns name diffs)
    Unchanged ns name ->
      assertBool
        ("did not expect record changes on " ++ show ns ++ " at " ++ show name)
        (not (hasChange ns name diffs))
    ServerChanged ns ->
      assertBool
        ("expected server changes on " ++ show ns)
        (maybe False (not . isEmpty) (M.lookup ns diffs))

hasChange :: Name String -> Name ConstrainedLabel -> NSDiffs -> Bool
hasChange ns name diffs =
  maybe False (hasChanges name) (M.lookup ns diffs)

assertQueries :: String -> S.Set ConstrainedQuery -> [Query] -> [Query] -> Assertion
assertQueries label results expected unexpected = do
  forM_ expected $ \query ->
    assertBool
      (label ++ " should include " ++ show query ++ "\n" ++ showResults results)
      (any (coversQuery query) results)
  forM_ unexpected $ \query ->
    assertBool
      (label ++ " should not include " ++ show query ++ "\n" ++ showResults results)
      (not (any (coversQuery query) results))

assertResolutions :: String -> S.Set ConstrainedQuery -> [Resolution] -> [Resolution] -> Assertion
assertResolutions label results expected unexpected = do
  forM_ expected $ \resolution ->
    assertBool
      (label ++ " should include " ++ show resolution ++ "\n" ++ showResults results)
      (any (coversResolution resolution) results)
  forM_ unexpected $ \resolution ->
    assertBool
      (label ++ " should not include " ++ show resolution ++ "\n" ++ showResults results)
      (not (any (coversResolution resolution) results))

showResults :: S.Set ConstrainedQuery -> String
showResults = unlines . map show . S.toList

coversQuery :: Query -> ConstrainedQuery -> Bool
coversQuery = coversTerm . queryTerm

coversResolution :: Resolution -> ConstrainedQuery -> Bool
coversResolution = coversTerm . resolutionTerm

coversTerm :: Term -> ConstrainedQuery -> Bool
coversTerm expected (CQuery actual constraints) =
  isJust $
    CU.extendUnifier
      actual
      expected
      (CU.CUnifier U.identity constraints)

queryTerm :: Query -> Term
queryTerm (Query ns rtype qname) =
  resolves
    (nameTerm ns)
    (ttypeTerm rtype)
    (nameTerm qname)
    TWildcard
    TWildcard
    TWildcard
    TWildcard

resolutionTerm :: Resolution -> Term
resolutionTerm (Resolution (Query ns rtype qname) verification auth) =
  resolves
    (nameTerm ns)
    (ttypeTerm rtype)
    (nameTerm qname)
    TWildcard
    TWildcard
    verification
    auth

resolvableQueries :: [FilePath] -> IO (S.Set ConstrainedQuery)
resolvableQueries files = do
  cfg <- loadConfig files
  let rules = runFresh $ plResolvable =<< compileConfig rootServers cfg
  resolvable <- assertRight (runFresh $ runExceptT $ computeResolvable rules)
  return $ S.fromList $ mapMaybe queryFromRule $ S.toList resolvable

loadConfig :: [FilePath] -> IO NameServersConfig
loadConfig = mapM loadConfigEntry
  where
    loadConfigEntry :: FilePath -> IO (Name String, [Zone ConstrainedLabel])
    loadConfigEntry path = do
      (ns, _, zones) <- loadExampleFile path
      forM_ zones $ assertWellformed path ns
      return (ns, map compileWildcards zones)

loadExampleFile :: FilePath -> IO (Name String, [StandardRecord], [Zone String])
loadExampleFile path = do
  content <- readFile path
  records <- assertRight =<< parseRecords path content
  ns <- assertRight =<< parseName path
  return (ns, records, fromRecordList records)

assertWellformed :: FilePath -> Name String -> Zone String -> Assertion
assertWellformed path ns zone =
  case wellformed ns zone of
    Nothing -> return ()
    Just err -> assertFailure (path ++ ": " ++ err)

assertRight :: Show e => Either e a -> IO a
assertRight = either (assertFailure . show) return
