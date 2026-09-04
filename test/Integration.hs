module Integration (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (isJust)
import qualified Data.Set as S

import DNS.Name (Name, root)
import DNS.NameSpec (mkName)
import qualified DNS.RecordType as RT
import DNS.Wellformedness (zoneWellformed)
import DNS.ZoneFile (zoneName)
import NameConstraint (exactly)
import Parsing.Config (parseZoneFileZones, parseZoneFiles)
import Resolvable.Dependencies (ConstrainedQuery (CQuery), computeResolvable, queryFromRule)
import Resolvable.Prolog.Core (Term, ad, nameTerm, resolvable, resolves, top, ttypeTerm, wildcard, wildcards3)
import Resolvable.Resolvability (NameServersConfig, compileConfig, plResolvable)
import Resolvable.Update.Dependencies (computeRegressions)
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Util.Fresh (runFresh)
import Util.Monad (failLeft, handleError)

tests :: TestTree
tests =
  testGroup
    "Examples"
    [ testGroup "Parsing/DNS.ZoneFile/Wellformedness" $
        map exampleFileTest exampleFiles
    , testGroup "Resolvable.Resolvability"
        (map exampleConfigTest exampleConfigs ++
          [ testCase "handles an NS delegation loop" $
              misconfigurationLoopTest nsLoopFiles
                [Query nsCom RT.NS com, Query rootNet RT.NS com]
                [Query nsCom RT.A aCom, Query rootNet RT.A aCom]
          , testCase "handles a CNAME loop" $
              misconfigurationLoopTest cnameLoopFiles
                [Query nsCom RT.CNAME aCom, Query nsCom RT.CNAME bCom]
                [Query nsCom RT.A aCom, Query nsCom RT.A bCom]
          ])
    , testGroup
        "Resolvable.Resolvability authentication"
        [ testCase "signed authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest signedFiles
        , testCase "signed-new authenticates answers synthesized from a wildcard" $
            authenticatedWildcardResolutionTest signedNewFiles
        , testCase "signed-nsec3 authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest signedNsec3Files
        , testCase "partially-signed authenticates unsigned child zone through NSEC" $
            authenticatedUnsignedDelegationTest partiallySignedFiles
        , testCase "partially-signed-nsec3 authenticates unsigned child zone through NSEC3 owner proof" $
            authenticatedUnsignedDelegationTest partiallySignedNsec3Files
        , testCase "partially-signed-nsec3-oo filters invalid NSEC3 unsigned-delegation proof" $
            doesNotAuthenticateUnsignedDelegationTest partiallySignedNsec3OOFiles
        , testCase "signed does not authenticate the child zone as unsigned through NSEC" $
            signedDelegationDoesNotUseUnsignedProofTest signedFiles
        , testCase "signed-nsec3 does not authenticate the child zone as unsigned through NSEC3" $
            signedDelegationDoesNotUseUnsignedProofTest signedNsec3Files
        , testCase "base does not authenticate unsigned answers" $ do
            results <- resolvableQueries baseFiles
            assertResolutions baseFiles results []
              [ authenticated nsCom RT.A aCom
              , authenticated nsCom RT.A bCom
              , authenticated nsCom RT.CNAME aCom
              , authenticated nsCom RT.NS bCom
              , authenticated nsBCom RT.A bCom ] ]
    , testGroup
        "Resolvable.Update.Dependencies"
        [ testCase "base to new has expected unsigned regression facts" $
            regressionTest newFiles
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.A aCom
              , Query nsCom RT.A bCom
              , Query nsCom RT.A nsBCom
              , Query nsCom RT.SOA com ]
        , testCase "base to broken has expected unsigned regression facts" $
            regressionTest brokenFiles
              [ Query nsCom RT.A aCom
              , Query nsCom RT.A bCom
              , Query nsCom RT.A nsBCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.SOA com ] ] ]

data ExampleFile = ExampleFile
  { examplePath :: FilePath
  , expectedZones :: [Name String]
  }

exampleFiles :: [ExampleFile]
exampleFiles =
  [ ExampleFile "examples/cname/base/ns.com." [com]
  , ExampleFile "examples/cname/base/ns.b.com." [bCom]
  , ExampleFile "examples/cname/new/ns.com." [com]
  , ExampleFile "examples/cname/broken/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed/ns.com." [com]
  , ExampleFile "examples/cname/signed/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed-new/ns.com." [com]
  , ExampleFile "examples/cname/signed-non-zone-key/ns.com." [com]
  , ExampleFile "examples/cname/signed-non-zone-key/ns.b.com." [bCom]
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.b.com." [bCom]
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed-nsec3/ns.com." [com]
  , ExampleFile "examples/cname/signed-nsec3/ns.b.com." [bCom]
  , ExampleFile "examples/ns/self-deleg/ns.com." [com, subACom]
  , ExampleFile "examples/ns/sub-self-deleg/root.net." [dnsRoot, rootNet]
  , ExampleFile "examples/ns/sub-self-deleg/ns.net." [net]
  , ExampleFile "examples/ns/cyclic-deleg/root.net." [dnsRoot]
  , ExampleFile "examples/ns/cyclic-deleg/ns.net." [net]
  , ExampleFile "examples/ns/cyclic-deleg/root.com." [dnsRoot] ]

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
      "signed-non-zone-key"
      signedNonZoneKeyFiles
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
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "ns/self-deleg"
      selfDelegFiles
      [ Query nsCom RT.NS com
      , Query nsCom RT.SOA com
      , Query nsCom RT.A nsCom
      , Query nsCom RT.NS subACom
      , Query nsCom RT.SOA subACom
      , Query nsCom RT.A subACom ]
      [Query nsCom RT.TXT subACom]
  , ExampleConfig
      "ns/sub-self-deleg"
      subSelfDelegFiles
      [ Query rootNet RT.NS dnsRoot
      , Query rootNet RT.SOA dnsRoot
      , Query rootNet RT.NS net
      , Query rootNet RT.A nsNet
      , Query nsNet RT.NS net
      , Query nsNet RT.SOA net
      , Query nsNet RT.NS rootNet
      , Query nsNet RT.A rootNet
      , Query rootNet RT.NS rootNet
      , Query rootNet RT.SOA rootNet
      , Query rootNet RT.A rootNet
      , Query nsNet RT.A subRootNet ]
      [Query rootNet RT.TXT subRootNet] ]

baseFiles, newFiles, brokenFiles, partiallySignedFiles, signedFiles, signedNewFiles, signedNonZoneKeyFiles :: [FilePath]
selfDelegFiles, subSelfDelegFiles, nsLoopFiles, cnameLoopFiles :: [FilePath]
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

signedNewFiles = ["examples/cname/signed-new/ns.com."]

signedNonZoneKeyFiles =
  [ "examples/cname/signed-non-zone-key/ns.com."
  , "examples/cname/signed-non-zone-key/ns.b.com." ]

partiallySignedNsec3Files =
  [ "examples/cname/partially-signed-nsec3/ns.com."
  , "examples/cname/partially-signed-nsec3/ns.b.com." ]

partiallySignedNsec3OOFiles =
  [ "examples/cname/partially-signed-nsec3-oo/ns.com."
  , "examples/cname/partially-signed-nsec3-oo/ns.b.com." ]

signedNsec3Files =
  [ "examples/cname/signed-nsec3/ns.com."
  , "examples/cname/signed-nsec3/ns.b.com." ]

selfDelegFiles = ["examples/ns/self-deleg/ns.com."]

subSelfDelegFiles =
  [ "examples/ns/sub-self-deleg/root.net."
  , "examples/ns/sub-self-deleg/ns.net." ]

nsLoopFiles =
  [ "examples/ns/loop/ns.com."
  , "examples/ns/loop/root.net." ]

cnameLoopFiles = ["examples/cname/loop/ns.com."]

data Query = Query (Name String) RT.Type (Name String)
  deriving Eq

instance Show Query where
  show (Query ns rtype qname) =
    "query " ++ show rtype ++ " " ++ show qname ++ " at name server " ++ show ns

data Resolution = Resolution Query Term Term
  deriving Eq

instance Show Resolution where
  show (Resolution query verification auth) =
    show query ++ " with verified=" ++ show verification ++ " and auth=" ++ show auth

authenticated :: Name String -> RT.Type -> Name String -> Resolution
authenticated ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 1)

authenticatedUnsigned :: Name String -> RT.Type -> Name String -> Resolution
authenticatedUnsigned ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 0)

dnsRoot, com, net, aCom, bCom, subACom :: Name String
rootNet, subRootNet, nsCom, nsBCom, nsNet :: Name String
dnsRoot = root
com = mkName ["com", ""]
net = mkName ["net", ""]
aCom = mkName ["a", "com", ""]
bCom = mkName ["b", "com", ""]
subACom = mkName ["sub", "a", "com", ""]
rootNet = mkName ["root", "net", ""]
subRootNet = mkName ["sub", "root", "net", ""]
nsCom = mkName ["ns", "com", ""]
nsBCom = mkName ["ns", "b", "com", ""]
nsNet = mkName ["ns", "net", ""]

rootServers :: [Name String]
rootServers = [nsCom, rootNet]

exampleFileTest :: ExampleFile -> TestTree
exampleFileTest example =
  testCase (examplePath example) $ do
    config <- parseZoneFileZones zoneWellformed (examplePath example)
    map (zoneName . snd) config
      @?= map exactly (expectedZones example)

exampleConfigTest :: ExampleConfig -> TestTree
exampleConfigTest example =
  testCase (configName example) $ do
    results <- resolvableQueries (configFiles example)
    assertQueries
      (configFiles example)
      results
      (expectedQueries example)
      (unexpectedQueries example)

misconfigurationLoopTest :: [FilePath] -> [Query] -> [Query] -> Assertion
misconfigurationLoopTest files expected unexpected = do
  results <- resolvableQueries files
  assertQueries files results expected unexpected

authenticatedResolutionTest :: [FilePath] -> Assertion
authenticatedResolutionTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated nsCom RT.A aCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.CNAME aCom
    , authenticated nsCom RT.DNSKEY com
    , authenticated nsCom RT.DNSKEY bCom
    , authenticated nsCom RT.DS bCom ]
    []

authenticatedWildcardResolutionTest :: [FilePath] -> Assertion
authenticatedWildcardResolutionTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated nsCom RT.A aCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DNSKEY com ]
    []

authenticatedUnsignedDelegationTest :: [FilePath] -> Assertion
authenticatedUnsignedDelegationTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

signedDelegationDoesNotUseUnsignedProofTest :: [FilePath] -> Assertion
signedDelegationDoesNotUseUnsignedProofTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]

doesNotAuthenticateUnsignedDelegationTest :: [FilePath] -> Assertion
doesNotAuthenticateUnsignedDelegationTest files = do
  results <- resolvableQueries files
  assertResolutions files results []
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

regressionTest :: [FilePath] -> [Query] -> [Query] -> Assertion
regressionTest newConfig expected unexpected = do
  old <- loadConfig baseFiles
  new <- loadConfig newConfig
  regressions <- failLeft $
    runFresh $
      runExceptT $
        computeRegressions
          (plResolvable =<< compileConfig rootServers old)
          (plResolvable =<< compileConfig rootServers new)
  assertResolvableQueries (baseFiles ++ newConfig) (S.fromList regressions) expected unexpected

assertQueries :: [FilePath] -> S.Set ConstrainedQuery -> [Query] -> [Query] -> Assertion
assertQueries = assertQueriesWith queryTerm

assertResolvableQueries :: [FilePath] -> S.Set ConstrainedQuery -> [Query] -> [Query] -> Assertion
assertResolvableQueries = assertQueriesWith resolvableQueryTerm

assertQueriesWith :: (Query -> Term) -> [FilePath] -> S.Set ConstrainedQuery -> [Query] -> [Query] -> Assertion
assertQueriesWith toTerm files results expected unexpected = do
  forM_ expected $ \query ->
    assertBool
      (show query ++ " was not resolvable for zones:\n" ++ showZoneFiles files)
      (any (coversTerm (toTerm query)) results)
  forM_ unexpected $ \query ->
    assertBool
      (show query ++ " was wrongly resolvable for zones:\n" ++ showZoneFiles files)
      (not (any (coversTerm (toTerm query)) results))

assertResolutions :: [FilePath] -> S.Set ConstrainedQuery -> [Resolution] -> [Resolution] -> Assertion
assertResolutions files results expected unexpected = do
  forM_ expected $ \resolution ->
    assertBool
      (show resolution ++ " was not resolvable for zones:\n" ++ showZoneFiles files)
      (any (coversResolution resolution) results)
  forM_ unexpected $ \resolution ->
    assertBool
      (show resolution ++ " was wrongly resolvable for zones:\n" ++ showZoneFiles files)
      (not (any (coversResolution resolution) results))

showZoneFiles :: [FilePath] -> String
showZoneFiles = unlines . map ("  " ++)

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
queryTerm (Query ns rtype qname) = runFresh $ do
  zone <- wildcard
  (wc1, wc2, wc3) <- wildcards3
  return $ resolves (nameTerm ns) (ttypeTerm rtype) zone (nameTerm qname) wc1 wc2 wc3

resolvableQueryTerm :: Query -> Term
resolvableQueryTerm (Query ns rtype qname) = runFresh $ resolvable (nameTerm ns) (ttypeTerm rtype) (nameTerm qname) <$> wildcard <*> wildcard

resolutionTerm :: Resolution -> Term
resolutionTerm (Resolution (Query ns rtype qname) verification auth) = runFresh $ do
  zone <- wildcard
  wc <- wildcard
  return $ resolves (nameTerm ns) (ttypeTerm rtype) zone (nameTerm qname) wc verification auth

resolvableQueries :: [FilePath] -> IO (S.Set ConstrainedQuery)
resolvableQueries files = do
  cfg <- loadConfig files
  let rules = runFresh $ plResolvable =<< compileConfig rootServers cfg
  let resolvableRules = computeResolvable True rules
  qs <- handleError (traverse queryFromRule $ S.toList resolvableRules)
  return $ S.fromList qs

loadConfig :: [FilePath] -> IO NameServersConfig
loadConfig = parseZoneFiles zoneWellformed
