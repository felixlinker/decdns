module Resolvable.Update.RolloutSpec (tests) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Except (runExcept)
import Data.List (foldl', intercalate, isSuffixOf, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen, Property, chooseInt, conjoin, counterexample
  , elements, forAll, frequency, label, once, suchThat, testProperty, vectorOf )

import DNS.Diff (deither)
import DNS.Name (Name, Named (..))
import DNS.Record (StandardRecord)
import qualified DNS.RecordType as RT
import qualified DNS.RRSet as RRSet
import DNS.Wellformedness (zoneWellformed)
import DNS.Zone (splitZone)
import DNS.ZoneFile (allData, allRecords, fromRecordList, mergeZones)
import DNS.ZoneFileSpec (genLabel)
import NameConstraint (compileWildcards, literalWildcards, toStandardRecord)
import Parsing (parseNameM)
import Parsing.Config (parseZoneFiles)
import qualified Resolvable.ConstrainedUnification as CU
import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Resolvable.Resolvability (NameServersConfig, compileConfig, plResolvable)
import Resolvable.Update.Rollout
import qualified Resolvable.Unification as U
import Util.Fresh (runFresh)
import qualified Util.Graph as G

-- Only the explicitly listed configurations participate in the pair matrix.
-- A directory is one complete configuration, even if it hosts several zones.
tests :: IO TestTree
tests = do
  configs <- exampleConfigurations
  when (null configs) (fail "no eligible rollout examples found")
  let examples = [Example path cfg (dependencies cfg) (queryPatterns cfg) | (path, cfg) <- configs]
  pure $ testGroup "Resolvable.Update.Rollout"
    [ testProperty (examplePath old ++ " -> " ++ examplePath new) (rolloutInvariant old new)
    | old <- examples, new <- examples
    ]

-- Share endpoint dependency and resolution computations across the whole matrix.
data Example = Example
  { examplePath :: FilePath
  , exampleConfig :: NameServersConfig
  , exampleDependencies :: Either String RecordDependencies
  , exampleQueries :: [ConstrainedQuery]
  }

-- Keep all rollout inputs here. Signing inputs, DNSSEC configurations, and
-- known cyclic scenarios are deliberately not part of this list.
exampleDirectories :: [FilePath]
exampleDirectories =
  [ "examples/cname/base"
  , "examples/cname/broken"
  , "examples/cname/new"
  , "examples/ns/self-deleg"
  , "examples/ns/sub-self-deleg"
  , "examples/rollout/address-to-alias-new"
  , "examples/rollout/address-to-alias-old"
  , "examples/rollout/alias"
  , "examples/rollout/colocated-child"
  , "examples/rollout/delayed-target-new"
  , "examples/rollout/delayed-target-old"
  , "examples/rollout/delegated-address"
  , "examples/rollout/delegated-wildcard"
  , "examples/rollout/direct-address"
  , "examples/rollout/dname"
  , "examples/rollout/wildcard"
  , "examples/rollout/wildcard-alias-txt"
  , "examples/rollout/wildcard-cname"
  , "examples/rollout/wildcard-cname-descendant"
  , "examples/rollout/wildcard-explicit"
  , "examples/rollout/wildcard-nested"
  , "examples/rollout/wildcard-txt"
  ]

exampleConfigurations :: IO [(FilePath, NameServersConfig)]
exampleConfigurations = mapM load exampleDirectories
  where
    load dir = do
      files <- map (dir </>) . filter ("." `isSuffixOf`) . sort <$> listDirectory dir
      when (null files) (fail ("no zone files in " ++ dir))
      cfg <- parseZoneFiles zoneWellformed files
      let unsigned = all (\(Named _ rs) -> all (\ty -> not (RRSet.hasType ty rs)) dnssecTypes)
            [rs | (_, z) <- cfg, rs <- allData z]
      unless unsigned (fail ("DNSSEC records in rollout configuration " ++ dir))
      pure (dir, cfg)
    dnssecTypes = [RT.DNSKEY, RT.RRSIG, RT.DS, RT.NSEC, RT.NSEC3, RT.NSEC3PARAM]

dnsName :: String -> Name String
dnsName n = fromMaybe (error ("invalid fixture name: " ++ n)) (parseNameM n)

roots :: [Name String]
roots = map dnsName ["ns.com.", "root.net."]

rolloutInvariant :: Example -> Example -> Property
rolloutInvariant old new = case prepare of
  Left err -> counterexample err False
  Right steps ->
    let initial = exampleQueries old
        final = exampleQueries new
        common = S.toList $ S.fromList
          [ q | a <- initial, b <- final, Just q <- [intersectQueries a b] ]
        states = drop 1 $ scanl applyStep (inventory (exampleConfig old)) steps
        intermediate = map (queryPatterns . toConfig) states
    in if null common
       then once $ label "empty endpoint intersection (vacuous invariant)" True
       else forAll (elements common >>= sampleQuery) $ \q ->
         counterexample ("Query: " ++ describeQuery q) $ conjoin $
           -- Also check the generator's contract; do not hide bad samples with ==>.
           counterexample "sample is not resolvable at both endpoints"
             (resolvesQuery initial q && resolvesQuery final q)
           : [ counterexample
                 ("Unresolvable after step " ++ show i ++ "\n" ++ unlines (showRollOut steps))
                 (resolvesQuery answers q)
             | (i, answers) <- zip [1 :: Int ..] intermediate
             ]
  where
    prepare = do
      oldDeps <- exampleDependencies old
      newDeps <- exampleDependencies new
      let oldConfig = exampleConfig old
          newConfig = exampleConfig new
      runExcept $ rollout (M.fromListWith mergeZones oldConfig)
        (diffServers oldConfig newConfig) oldDeps newDeps

-- Endpoint validation is fixture validation, not an intermediate assertion:
-- this property tests query continuity, not zone validity or answer equality.
dependencies :: NameServersConfig -> Either String RecordDependencies
dependencies cfg = do
  mapM_ (\(_, z) -> maybe (Right ()) Left (zoneWellformed (literalWildcards z))) cfg
  let order = runFresh $ resolutionOrder <$> compileConfig roots cfg
  unless (isJust (G.transitiveClosure order)) (Left "cyclic fixture rule order")
  maybe (Left "cyclic fixture record order") Right (fromResolutionOrder order)

-- Project away response data and the answering zone BEFORE intersecting: an
-- address, alias target, or authoritative zone may legitimately change.
queryPatterns :: NameServersConfig -> [ConstrainedQuery]
queryPatterns cfg = S.toList $ S.fromList $ mapMaybe project
  [ (answer, ns, ty) | answer <- answers, ns <- roots,
    ty <- [RT.A, RT.AAAA, RT.NS, RT.CNAME, RT.DNAME, RT.TXT, RT.MX, RT.SOA] ]
  where
    answers = either error id $ runExcept $ traverse queryFromRule $ S.toList $
      computeResolvable True $ runFresh $ plResolvable =<< compileConfig roots cfg
    project (answer, ns, ty) = do
      let CQuery h cs = renameQuery "answer_" answer
          q = TVar "query_name"
          probe = resolves (nameTerm ns) (ttypeTerm ty) (TVar "query_zone") q
            (TVar "query_data") bot (ad 0)
      cu <- CU.extendUnifier h probe (CU.CUnifier U.identity cs)
      pure $ CQuery (TFunctor "query" [nameTerm ns, ttypeTerm ty, CU.apply cu q]) (CU.constraints cu)

intersectQueries :: ConstrainedQuery -> ConstrainedQuery -> Maybe ConstrainedQuery
intersectQueries a b = do
  let CQuery h cs = renameQuery "old_" a
      CQuery h' cs' = renameQuery "new_" b
  cu <- CU.extendUnifier h h' (CU.CUnifier U.identity (M.unionWith S.union cs cs'))
  pure $ CQuery (CU.apply cu h) (CU.constraints cu)

-- Independently compiled configurations reuse variable names. Renaming both
-- terms AND constraints apart avoids accidental links between their variables.
renameQuery :: String -> ConstrainedQuery -> ConstrainedQuery
renameQuery prefix (CQuery h cs) = CQuery (U.apply u h) renamedConstraints
  where
    terms = h : concatMap (\(a, b) -> [a, b]) (CU.toList cs)
    variables = S.unions (map vars terms)
    u = U.Unifier $ M.fromList [(v, TVar (prefix ++ n)) | v@(TVar n) <- S.toList variables]
    renamedConstraints = fromMaybe (error "invalid query constraints") $
      CU.fromList (U.apply u (CU.toList cs))

resolvesQuery :: [ConstrainedQuery] -> Term -> Bool
resolvesQuery answers q = any (covers q) answers

covers :: Term -> ConstrainedQuery -> Bool
covers q (CQuery h cs) = isJust $ CU.extendUnifier h q (CU.CUnifier U.identity cs)

-- Sample a concrete DNS name from a symbolic intersection member, including
-- wildcard suffixes. Constraint checking rejects excluded names, not properties.
-- Fresh labels ensure exclusions cannot exhaust the small genLabel alphabet.
sampleQuery :: ConstrainedQuery -> Gen Term
sampleQuery patternQuery@(CQuery (TFunctor "query" [ns, ty, n]) _) =
  gen `suchThat` (`covers` patternQuery)
  where
    (labels, rest) = collectListWithTail n
    genNameLabel = frequency [(1, genLabel), (1, ("probe" ++) . show <$> chooseInt (0, 1000000))]
    instantiate (TVar _) = TAtom <$> genNameLabel
    instantiate t = pure t
    gen = do
      fixed <- mapM instantiate labels
      suffix <- case rest of
        Nothing -> pure []
        Just (TVar _) -> chooseInt (0, 3) >>= \k -> vectorOf k (TAtom <$> genNameLabel)
        _ -> error "unexpected DNS name tail"
      pure $ TFunctor "query" [ns, ty, list (fixed ++ suffix)]
sampleQuery _ = error "unexpected projected query"

describeQuery :: Term -> String
describeQuery (TFunctor "query" [ns, ty, n]) = nameString n ++ " " ++ show ty ++ " at " ++ nameString ns
  where
    nameString t = case collectList t of
      Just labels -> intercalate "." (reverse (map show labels)) ++ "."
      Nothing -> show t
describeQuery t = show t

type Inventory = M.Map (Name String) (S.Set StandardRecord)

inventory :: NameServersConfig -> Inventory
inventory cfg = M.fromListWith S.union
  [(ns, S.fromList (map toStandardRecord (allRecords z))) | (ns, z) <- cfg]

-- Replay exactly the emitted changes, including filler records. Calling the
-- synthesizing patcher to create additional changes here would mask rollout bugs.
applyStep :: Inventory -> Diffs -> Inventory
applyStep inv step = M.filter (not . S.null) $ foldl' applyOwner inv (S.toList step)
  where
    applyOwner acc (ns, Named owner changes) = M.insert ns
      (foldl' change (M.findWithDefault S.empty ns acc)
        [fmap (Named owner) d | d <- RRSet.flattenData changes]) acc
    change records = deither (`S.delete` records) (`S.insert` records)

toConfig :: Inventory -> NameServersConfig
toConfig inv =
  [(ns, compileWildcards z) | (ns, records) <- M.toList inv,
    Just rs <- [NE.nonEmpty (S.toList records)], z <- splitZone (fromRecordList rs)]
