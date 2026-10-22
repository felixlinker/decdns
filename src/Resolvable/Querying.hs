{-# LANGUAGE ViewPatterns #-}
-- | SLD resolution with a per-run, per-zone rule base populated by direct DNS queries.
module Resolvable.Querying
  ( QueryConfig(..)
  , Query
  , defaultQueryConfig
  , resolve
  , resolveWith
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Trans.Except as Ex
import qualified Data.ByteString.Char8 as BS
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.IP (IP(..))
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Ord (Down(Down))
import Data.List (sortOn, uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.DNS as W

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.Response
import qualified DNS.ZoneFile as Z
import qualified DNS.RRSet as RRSet
import Resolvable.Resolvability (compileConfig, RComp (fromType), nsesTerm)
import Resolvable.Prolog.Core
import Resolvable.Prolog.Reverse (asName, asTType)
import Resolvable.Prolog.TermView
import Resolvable.Resolution
import Resolvable.Unification hiding (toList)
import Util.Fresh
import Util.Monad (maybeExcept, first)
import Util.List (snub)
import NameConstraint (compileWildcardsLiteral)
import DNS.RRSet (RRSet)
import DNS.ZoneFile (fromRecordList)
import Util.Map (add, groupBy, groupByWith)
import Data.Foldable (Foldable(toList))

data QueryConfig = QueryConfig
  { queryPort :: Int
  , queryTimeout :: Int  -- ^ Microseconds per attempt.
  , queryRetries :: Int
  , queryLimit :: Int  -- ^ Lookup budget, shared across servers and addresses.
  , _resolutionLimits :: M.Map T.Type Int
  , qtrace :: String -> IO ()
  }

resolutionLimits :: QueryConfig -> M.Map (Maybe T.Type) Int
resolutionLimits = M.mapKeys Just . _resolutionLimits

defaultQueryConfig :: QueryConfig
defaultQueryConfig = QueryConfig 53 3000000 2 100 limits putStrLn
  where
    limits :: M.Map T.Type Int
    limits = M.fromList [(T.CNAME, 10), (T.DNAME, 10), (T.NS, 20)]

type Origin = S.Set (Name String)

rOrigin :: Foldable f => f (Record a) -> Origin
rOrigin = S.fromList . RRSet.nsNames . map value . toList

data Query = Query Origin (Name String, T.Type, Name String)
  deriving (Eq, Ord)

instance Show Query where
  show (Query _ (ns, t, n)) = unwords ['@' : show ns, show t, show n]

data QueryState = QueryState
  { dnsData :: ZoneCache
  , queried :: [Query]
  , trustAnchors :: [RRSet String]
  , config :: QueryConfig
  , remainingQueries :: Int
  }

type RuleBase = Ex.ExceptT String (St.StateT QueryState IO)

liftIO :: IO a -> RuleBase a
liftIO = lift . lift

type OnlineSolver a = RSolver (Maybe T.Type) RuleBase a

type OnlineGoal = BoundedGoal (Maybe T.Type)

runOnlineSolverT :: QueryState -> OnlineSolver a -> IO (Either String a, [Query], [Z.Zone String])
runOnlineSolverT qst0 slv =
  let solved = St.runStateT (Ex.runExceptT $ fst <$> runSLD slv) qst0
  in do
    (sol, qst) <- solved
    return (sol, queried qst, cachedZones (dnsData qst))

getsState :: (QueryState -> a) -> RuleBase a
getsState = lift . St.gets

modState :: (QueryState -> QueryState) -> RuleBase ()
modState = lift . St.modify

whenNotQueried :: Query -> RuleBase Bool -> RuleBase Bool
whenNotQueried q m = do
  qs <- getsState queried
  if q `notElem` qs
    then modState (\qst -> qst { queried = q : queried qst }) >> m
    else return False

checkQueryBudget :: RuleBase ()
checkQueryBudget = do
  qs <- getsState remainingQueries
  when (qs <= 0) (Ex.throwE "exhausted query budget")
  modState (\qst -> qst { remainingQueries = qs - 1 })

liftRuleBase :: RuleBase a -> OnlineSolver a
liftRuleBase = lift . lift . lift

getRules :: RuleBase (S.Set (Rule (Maybe T.Type)))
getRules = do
  qst <- lift St.get
  let dat = dnsData qst
      rootNSes = nameServers root dat
      zones = map compileWildcardsLiteral $ cachedZones dat
  compiled <- either Ex.throwE return $ runFresh $ Ex.runExceptT $ compileConfig root (trustAnchors qst) zones
  -- Can use map monotonic because rule ordering ignores annotation
  let rs = S.mapMonotonic annot compiled
  return $ S.insert (rootRule rootNSes) rs
  where
    annot :: Rule RComp -> Rule (Maybe T.Type)
    annot = fmap (Just . fromType)

newtype ZoneCache = ZoneCache
  { recordsByZone :: M.Map (Origin, Name String) (S.Set StandardRecord) }

emptyZoneCache :: ZoneCache
emptyZoneCache = ZoneCache M.empty

-- TODO: This code will not work well when the set of name servers at the parent
-- and child side of a delegation differs.
cacheRecords :: Origin -> [StandardRecord] -> ZoneCache -> ZoneCache
cacheRecords (S.map canonicalName -> origin) (map canonicalRecord -> records) cache =
  let cache' = foldr (uncurry newZone) cache newApices
  in  foldr cacheRecord cache' records
  where
    newApices :: [((Origin, Name String), S.Set StandardRecord)]
    newApices = map (\(n, nses) -> ((rOrigin nses, n), nses))
      $ M.toList
      $ M.map S.fromList
      $ groupBy name (filter ((== T.NS) . recordType) records)

    closestApex :: Name String -> ZoneCache -> Maybe (Name String)
    closestApex n (ZoneCache m) =
        listToMaybe
      $ sortOn (Down . length)
      $ map snd
      $ filter (uncurry (&&) . bimap (== origin) (isSubDomain n))
      $ M.keys m

    -- This may drop additional data, but that is okay
    cacheRecord :: StandardRecord -> ZoneCache -> ZoneCache
    cacheRecord r@(Named n _) z@(ZoneCache m) = maybe z
      (\apex -> ZoneCache $ add (origin, apex) (S.singleton r) m)
      (closestApex n z)

    newZone :: (Origin, Name String) -> S.Set StandardRecord -> ZoneCache -> ZoneCache
    newZone k rs (ZoneCache m) = ZoneCache $ add k rs m

cachedRecords :: ZoneCache -> [StandardRecord]
cachedRecords = concatMap S.toList . M.elems . recordsByZone

nameServers :: Name String -> ZoneCache -> [Name String]
nameServers owner (ZoneCache cached) = maybe [] S.toList $ do
  (nses, rest) <- uncons $ map fst $ filter ((== owner) . snd) $ M.keys cached
  -- Assume that there is only one set of servers per zone
  unless (null rest) (error "invariant error")
  return nses

cachedZones :: ZoneCache -> [Z.Zone String]
cachedZones (recordsByZone -> M.elems -> cache) =
  mapMaybe (fmap fromRecordList . NE.nonEmpty . S.toList) cache

knownAddresses :: ZoneCache -> M.Map (Name String) [IP]
knownAddresses cache =
  groupByWith id $ concatMap (RRSet.getIP . fmap NE.singleton) $ cachedRecords cache

nsAddrs :: Name String -> ZoneCache -> [IP]
nsAddrs n zc = M.findWithDefault [] n (knownAddresses zc)

goalToQueries :: OnlineGoal -> [Query]
goalToQueries (Goal _ [] _) = []
goalToQueries (Goal _ (h:_) _) = case toView h of
  TVResolves r -> snub $ mapMaybe
    (\ns -> maybeExcept $ Query origin <$> ((,,) <$> asName ns <*> asTType (resolvesQType r) <*> asName (resolvesQName r)))
    nses
    where
      nses = fromMaybe [] (collectList (resolvesNS r))
      origin = S.fromList (mapMaybe (maybeExcept . asName) nses)
  _ -> []

dnsLookup :: OnlineGoal -> RuleBase Bool
dnsLookup = (`first` lookupQuery) . goalToQueries
  where
    lookupQuery q@(Query _ (ns, _, _)) = do
      qst <- lift St.get
      whenNotQueried q (first (nsAddrs ns (dnsData qst)) (dnsQuery q))

dnsQuery :: Query -> IP -> RuleBase Bool
dnsQuery q@(Query origin (_, qt, qn)) ip = do
  cfg <- getsState config
  checkQueryBudget
  liftIO $ qtrace cfg $ unwords ["% Querying: ", show q]
  result <- liftIO $ queryServer cfg ip qt qn
  either  (const $ return False)
          (\resp -> do
              let records = mapMaybe fromResourceRecord (W.answer resp ++ W.authority resp ++ W.additional resp)
              liftIO $ do
                qtrace cfg "% Caching data:"
                mapM_ (\r -> qtrace cfg $ unwords ["%   ", show r]) records
              modState (\st -> st { dnsData = cacheRecords origin records (dnsData st) })
              return True)
          result

queryServer :: QueryConfig -> IP -> T.Type -> Name String -> IO (Either W.DNSError W.DNSMessage)
queryServer cfg ip qt qn = do
  seed <- W.makeResolvSeed W.defaultResolvConf
    { W.resolvInfo = W.RCHostPort (show ip) (fromIntegral (queryPort cfg))
    , W.resolvTimeout = queryTimeout cfg
    , W.resolvRetry = queryRetries cfg
    , W.resolvCache = Nothing
    }
  W.withResolver seed $ \res -> W.lookupRawCtl res (BS.pack (show qn))
    (W.toTYPE (fromIntegral (T.typeToInt qt))) (W.rdFlag W.FlagClear <> W.doFlag W.FlagSet)

queryT :: Term -> Term -> Term -> Term -> Term -> Term
queryT q n d v a = TFunctor "query" [q, n, d, v, a]

queryG :: Limits (Maybe T.Type) -> T.Type -> Name String -> (Term, OnlineGoal, Unifier)
queryG limits qt qn =
  let q = queryT (ttypeTerm qt) (nameTerm $ canonicalName qn) (TVar "Data") top (TVar "Validated")
      (g0, u0) = goal limits q
  in  (q, g0, u0)

rootRule :: [Name String] -> Rule (Maybe T.Type)
rootRule nses =
  let q = TVar "Q"; n = TVar "N"; d = TVar "D"; v = TVar "V"; a = TVar "A"; z = TVar "Z"; alias = TVar "Alias"
  in Rule Nothing (queryT q n d v a)
       [resolves (nsesTerm nses) q z n d alias v a, z \= referralZone]

sortFailures :: [Query] -> T.Type -> Name String -> [OnlineGoal] -> [OnlineGoal]
sortFailures qs qt qn = sortOn priority
  where
    -- Highest priority will get queried last.
    priority :: OnlineGoal -> (Bool, Int, Int, Int)
    priority failed = case goalToQueries failed of
      [] -> (True, 3, 0, 0)
      candidates@(Query _ (_, t, n):_) ->
        (any (`elem` qs) candidates, group t, qnameDistance n, typePriority t)

    group :: T.Type -> Int
    group T.DNSKEY = 0
    group t | t `elem` [T.NSEC, T.NSEC3, T.NSEC3PARAM] = 2
    group _ = 1

    qnameDistance :: Name String -> Int
    qnameDistance n =
      let common = maybe 0 length (intersection qn n)
      in length qn + length n - 2 * common

    typePriority :: T.Type -> Int
    typePriority t | t == qt = 0
    typePriority T.A = 1
    typePriority T.AAAA = 1
    typePriority _ = 2

runOnline :: T.Type -> Name String -> OnlineGoal -> Unifier -> OnlineSolver (Maybe Unifier)
runOnline qt qn g u = do
  rs <- liftRuleBase getRules
  us <- sldSome (S.toList rs) g u
  failures <- clearFailures
  maybe (rec failures) (return . Just) us
  where
    rec :: S.Set OnlineGoal -> OnlineSolver (Maybe Unifier)
    rec failures = do
      discovered <- liftRuleBase $ do
        qs <- getsState queried
        first (sortFailures qs qt qn (S.toList failures)) dnsLookup
      if discovered then runOnline qt qn g u else return Nothing

resolve :: [RRSet String] -> [RRSet String] -> T.Type -> Name String -> IO (Maybe Term, [Query], [Z.Zone String])
resolve = resolveWith defaultQueryConfig

resolveWith :: QueryConfig -> [RRSet String] -> [RRSet String] -> T.Type -> Name String -> IO (Maybe Term, [Query], [Z.Zone String])
resolveWith cfg anchors hints qt qn
  | queryPort cfg < 1 || queryPort cfg > 65535 = fail "DNS port must be between 1 and 65535"
  | queryTimeout cfg <= 0 || queryRetries cfg <= 0 = fail "DNS timeout and retries must be positive"
  | queryLimit cfg < 0 = fail "DNS query limit must be non-negative"
  | otherwise = do
      let (q, g0, u0) = queryG (resolutionLimits cfg) qt qn
      zc <- zoneCache
      (result, qs, zs) <- runOnlineSolverT (qst zc) $ runOnline qt qn g0 u0
      answer <- either (\err -> putStrLn (unwords ["error:", err]) $> Nothing) return result
      return ((`apply` q) <$> answer, qs, zs)
    where
      zoneCache :: IO ZoneCache
      zoneCache =
        let records = concatMap (NE.toList . RRSet.toRecords . RRSet.canonicalRRSet) hints
            roots = rOrigin $ filter ((== root) . name) records
        in do
          when (S.null roots) $ fail "Root hints must contain root NS records"
          return $ cacheRecords roots records emptyZoneCache

      qst :: ZoneCache -> QueryState
      qst zc = QueryState zc [] anchors cfg (queryLimit cfg)
