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
  , elements, forAll, forAllShrink, frequency, label, once, shrinkList
  , sublistOf, suchThat, testProperty, vectorOf )

import DNS.Diff (deither)
import DNS.Name (Name, Named (..))
import DNS.Record (StandardRecord, rtype, toType)
import qualified DNS.RecordType as RT
import qualified DNS.RRSet as RRSet
import DNS.Wellformedness (isFatal, zoneWellformed)
import DNS.Zone (splitZone)
import DNS.ZoneFile (allData, allRecords, fromRecordList, mergeZones)
import DNS.ZoneFileSpec (genLabel)
import NameConstraint (compileWildcards, literalWildcards, toStandardRecord)
import Parsing (parseNameM)
import Parsing.Config (parseZoneFiles)
import Resolvable.Dependencies
import qualified Resolvable.Constraint.Solving as Solving
import qualified Resolvable.Constraint.Store as Store
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
      pairs = [(old, new, prepareRollout old new) | old <- examples, new <- examples]
      matrix invariant =
        [ testProperty (examplePath old ++ " -> " ++ examplePath new) (invariant old new prepared)
        | (old, new, prepared) <- pairs
        ]
  pure $ testGroup "Resolvable.Update.Rollout"
    [ testGroup "query continuity" (matrix rolloutInvariant)
    , testGroup "outdated cache" (matrix cacheInvariant)
    ]

-- Share endpoint dependency and resolution computations across the whole matrix.
data Example = Example
  { examplePath :: FilePath
  , exampleConfig :: NameServersConfig
  , exampleDependencies :: Either String RecordDependencies
  , exampleQueries :: [Fact]
  }

-- Keep all rollout inputs here. Wildcard owners, signing inputs, DNSSEC
-- configurations, and known cyclic scenarios are deliberately excluded.
exampleDirectories :: [FilePath]
exampleDirectories =
  [ "examples/cname/base"
  , "examples/cname/broken"
  , "examples/ns/self-deleg"
  , "examples/ns/sub-self-deleg"
  , "examples/rollout/address-to-alias-new"
  , "examples/rollout/address-to-alias-old"
  , "examples/rollout/alias"
  , "examples/rollout/alias-descendant-chain"
  , "examples/rollout/alias-descendant-txt"
  , "examples/rollout/colocated-child"
  , "examples/rollout/delayed-target-new"
  , "examples/rollout/delayed-target-old"
  , "examples/rollout/delegated-address"
  , "examples/rollout/delegated-wildcard"
  , "examples/rollout/direct-address"
  , "examples/rollout/dname"
  , "examples/rollout/dname-chain"
  , "examples/rollout/dname-nested"
  , "examples/rollout/nested-alias"
  , "examples/rollout/nested-alias-target"
  , "examples/rollout/nested-delegation"
  , "examples/rollout/nested-wildcard-alias"
  , "examples/rollout/wildcard"
  , "examples/rollout/wildcard-alias-txt"
  , "examples/rollout/wildcard-cname"
  , "examples/rollout/wildcard-cname-descendant"
  , "examples/rollout/wildcard-dname-target"
  , "examples/rollout/wildcard-explicit"
  , "examples/rollout/wildcard-nested"
  , "examples/rollout/wildcard-other-address"
  , "examples/rollout/wildcard-target-alias"
  , "examples/rollout/wildcard-txt"
  , "examples/rollout/unchanged-cname-new"
  , "examples/rollout/unchanged-cname-old"
  , "examples/rollout/unchanged-delegation-new"
  , "examples/rollout/unchanged-delegation-old"
  , "examples/rollout/unchanged-dname-new"
  , "examples/rollout/unchanged-dname-old"
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

-- Prepare each pair once for both invariants, and share resolution computations
-- for adjacent states. Include the initial and final states, even for one step.
type PreparedRollout = Either String (Rollout, [(Inventory, [Fact])])

prepareRollout :: Example -> Example -> PreparedRollout
prepareRollout old new = do
  oldDeps <- exampleDependencies old
  newDeps <- exampleDependencies new
  steps <- runExcept $ rollout
    (M.fromListWith mergeZones (exampleConfig old))
    (M.fromListWith mergeZones (exampleConfig new))
    oldDeps newDeps
  let initial = (inventory (exampleConfig old), exampleQueries old)
      states = drop 1 $ scanl applyStep (fst initial) steps
  pure (steps, initial : [(inv, queryPatterns (toConfig inv)) | inv <- states])

rolloutInvariant :: Example -> Example -> PreparedRollout -> Property
rolloutInvariant old new prepared = case prepared of
  Left err -> counterexample err False
  Right (steps, states) ->
    let initial = exampleQueries old
        final = exampleQueries new
        intermediate = map snd $ take (length steps - 1) $ drop 1 states
    in forCommonQueries initial final $ \genQuery -> forAll genQuery $ \q ->
      counterexample ("Query: " ++ describeQuery q) $ conjoin $
        sampleResolves initial final q
        : [ counterexample
              ("Unresolvable after step " ++ show i ++ "\n" ++ unlines (showRollOut steps))
              (resolvesQuery answers q)
          | (i, answers) <- zip [1 :: Int ..] intermediate
          ]

-- Both endpoints are required: old-only queries may be intentionally removed,
-- while new-only queries have no pre-update continuity guarantee. This tests
-- stale positive RRsets, not cached NXDOMAIN/NODATA responses or TTL expiry.
cacheInvariant :: Example -> Example -> PreparedRollout -> Property
cacheInvariant old new prepared = case prepared of
  Left err -> counterexample err False
  Right (steps, states) ->
    if null steps
    then once $ label "no transitions (identical configurations)" $
      counterexample "empty rollout for distinct configurations"
        (inventory (exampleConfig old) == inventory (exampleConfig new))
    else conjoin
      [ counterexample ("Transition " ++ show i ++ " -> " ++ show (i + 1)
          ++ "\n" ++ unlines (showRollOut steps)) $
          forCommonQueries beforeAnswers afterAnswers $ \genQuery ->
            let beforeRRSets = rrsetInventory before
                afterRRSets = rrsetInventory after
                changed = M.keys $ M.differenceWith
                  (\a b -> if a == b then Nothing else Just a) beforeRRSets afterRRSets
                gen = (,) <$> genCache changed <*> genQuery
                -- Shrink the cache without changing the failing query.
                shrinkCache (cached, q) = [(cached', q) | cached' <- shrinkList (const []) cached]
            in forAllShrink gen shrinkCache $ \(cached, q) ->
              let mixed = withOutdatedCache beforeRRSets afterRRSets cached
                  mixedAnswers = if null cached then afterAnswers else queryPatterns (toConfig mixed)
              in counterexample ("Query: " ++ describeQuery q
                  ++ "\nCached RRsets (server, owner, type): " ++ show cached) $
                conjoin
                  [ sampleResolves beforeAnswers afterAnswers q
                  , counterexample "unresolvable with outdated cache"
                      (resolvesQuery mixedAnswers q)
                  ]
      | (i, ((before, beforeAnswers), (after, afterAnswers))) <-
          zip [0 :: Int ..] (zip states (drop 1 states))
      ]

forCommonQueries :: [Fact] -> [Fact] -> (Gen Term -> Property) -> Property
forCommonQueries before after check =
  let common = S.toList $ S.fromList
        [ q | a <- before, b <- after, Just q <- [intersectQueries a b] ]
  in if null common
     -- Do not use once here: a vacuous transition inside conjoin must not
     -- reduce the number of samples for other, non-vacuous transitions.
     then label "empty endpoint intersection (vacuous invariant)" True
     else check (elements common >>= sampleQuery)

-- Check the generator's contract; do not hide bad samples with ==>.
sampleResolves :: [Fact] -> [Fact] -> Term -> Property
sampleResolves before after q = counterexample "sample is not resolvable at both endpoints"
  (resolvesQuery before q && resolvesQuery after q)

type CacheKey = (Name String, Name String, RT.Type)
type RRSetInventory = M.Map CacheKey (S.Set StandardRecord)

rrsetInventory :: Inventory -> RRSetInventory
rrsetInventory inv = M.fromListWith S.union
  [ ((ns, owner, toType (rtype rd)), S.singleton r)
  | (ns, records) <- M.toList inv, r@(Named owner rd) <- S.toList records
  ]

-- Selecting only changed, previously present RRsets is sufficient: unchanged
-- RRsets have no effect, and absence is not a positive cache entry. Restore
-- deleted RRsets too. Never mix individual records of a single RRset.
genCache :: [CacheKey] -> Gen [CacheKey]
genCache [] = pure []
genCache keys = frequency
  [ (1, pure [])
  , (1, pure keys)
  , (2, (:[]) <$> elements keys)
  , (6, sublistOf keys)
  ]

withOutdatedCache :: RRSetInventory -> RRSetInventory -> [CacheKey] -> Inventory
withOutdatedCache before after cached = M.fromListWith S.union
  [ (ns, records)
  | ((ns, _, _), records) <- M.toList $
      M.union (M.restrictKeys before (S.fromList cached)) after
  ]

-- Endpoint validation is fixture validation, not an intermediate assertion:
-- this property tests query continuity, not zone validity or answer equality.
dependencies :: NameServersConfig -> Either String RecordDependencies
dependencies cfg = do
  mapM_ checkWellformed cfg
  let order = runFresh $ resolutionOrder <$> compileConfig roots cfg
  unless (isJust (G.transitiveClosure order)) (Left "cyclic fixture rule order")
  Right (fromResolutionOrder order)
  where
    checkWellformed (_, zone) =
      case filter isFatal (zoneWellformed (literalWildcards zone)) of
        [] -> Right ()
        issues -> Left (unlines (map show issues))

-- Project away response data and the answering zone BEFORE intersecting: an
-- address, alias target, or authoritative zone may legitimately change.
queryPatterns :: NameServersConfig -> [Fact]
queryPatterns cfg = S.toList $ S.fromList $ mapMaybe project
  [ (answer, ns, ty) | answer <- S.toList answers, ns <- roots,
    ty <- [RT.A, RT.AAAA, RT.NS, RT.CNAME, RT.DNAME, RT.TXT, RT.MX, RT.SOA] ]
  where
    answers = runFresh $ do
      rules <- plResolvable =<< compileConfig roots cfg
      computeResolvable True rules
    project (answer, ns, ty) = do
      let Fact h cs = renameQuery "answer_" answer
          q = TVar "query_name"
          probe = resolves (nameTerm ns) (ttypeTerm ty) (TVar "query_zone") q
            (TVar "query_data") bot (ad 0)
      u <- U.extendUnifier h probe U.identity
      constraints <- Solving.getSolved $ Store.simplify (U.apply u cs)
      pure $ Fact (TFunctor "query" [nameTerm ns, ttypeTerm ty, U.apply u q]) constraints

intersectQueries :: Fact -> Fact -> Maybe Fact
intersectQueries a b = do
  let Fact h cs = renameQuery "old_" a
      Fact h' cs' = renameQuery "new_" b
  u <- U.extendUnifier h h' U.identity
  constraints <- Solving.getSolved $ Store.simplify (U.apply u (cs <> cs'))
  pure $ Fact (U.apply u h) constraints

-- Independently compiled configurations reuse variable names. Renaming both
-- terms AND constraints apart avoids accidental links between their variables.
renameQuery :: String -> Fact -> Fact
renameQuery prefix (Fact h cs) = Fact (U.apply u h) (U.apply u cs)
  where
    terms = h : Store.toBody cs
    variables = S.unions (map vars terms)
    u = U.Unifier $ M.fromList [(v, TVar (prefix ++ n)) | v@(TVar n) <- S.toList variables]

resolvesQuery :: [Fact] -> Term -> Bool
resolvesQuery answers q = any (covers q) answers

covers :: Term -> Fact -> Bool
covers q (Fact h cs) = isJust $ do
  u <- U.extendUnifier h q U.identity
  Solving.getSolved $ Store.simplify (U.apply u cs)

-- Sample a concrete DNS name from a symbolic intersection member, including
-- wildcard suffixes. Constraint checking rejects excluded names, not properties.
-- Fresh labels ensure exclusions cannot exhaust the small genLabel alphabet.
sampleQuery :: Fact -> Gen Term
sampleQuery patternQuery@(Fact (TFunctor "query" [ns, ty, n]) _) =
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
applyStep inv step = M.filter (not . S.null) $ foldl' applyOwner inv (M.toList step)
  where
    applyOwner acc ((ns, owner), changes) = M.insert ns
      (foldl' change (M.findWithDefault S.empty ns acc)
        [fmap (Named owner) d | d <- S.toList changes]) acc
    change records = deither (`S.delete` records) (`S.insert` records)

toConfig :: Inventory -> NameServersConfig
toConfig inv =
  [(ns, compileWildcards z) | (ns, records) <- M.toList inv,
    Just rs <- [NE.nonEmpty (S.toList records)], z <- splitZone (fromRecordList rs)]
