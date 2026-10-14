{-# LANGUAGE ViewPatterns #-}
-- | SLD resolution with a per-run rule base populated by direct DNS queries.
module Resolvable.Querying
  ( QueryConfig(..)
  , Query
  , defaultQueryConfig
  , resolve
  , resolveWith
  ) where

import Control.Monad (unless, when, (>=>), join)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.Trans.Except as Ex
import qualified Data.ByteString.Char8 as BS
import Data.Bifunctor (Bifunctor(bimap))
import Data.IP (IP(..))
import Data.Maybe (mapMaybe, isJust)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.DNS as W

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.Response
import DNS.Zone (splitZones)
import Resolvable.Resolvability (compileConfig, RComp (fromType))
import Resolvable.Prolog.Core
import Resolvable.Prolog.Reverse (asName, asTType)
import Resolvable.Prolog.TermView
import Resolvable.Resolution
import Resolvable.Unification
import Util.Fresh
import Util.Monad (maybeExcept, first, firstP)
import Util.Map (add)
import Util.List (snub)
import NameConstraint (compileWildcardsLiteral)
import DNS.RRSet (RRSet)

data QueryConfig = QueryConfig
  { queryPort :: Int
  , queryTimeout :: Int  -- ^ Microseconds per attempt.
  , queryRetries :: Int
  , queryLimit :: Int  -- ^ Lookup budget, shared across servers and addresses.
  , _resolutionLimits :: M.Map T.Type Int
  } deriving (Eq, Show)

resolutionLimits :: QueryConfig -> M.Map (Maybe T.Type) Int
resolutionLimits = M.mapKeys Just . _resolutionLimits

defaultQueryConfig :: QueryConfig
defaultQueryConfig = QueryConfig 53 3000000 2 100 limits
  where
    limits :: M.Map T.Type Int
    limits = M.fromList [(T.CNAME, 10), (T.DNAME, 10), (T.NS, 20)]

newtype Query = Query (Name String, T.Type, Name String)
  deriving (Eq, Ord)

instance Show Query where
  show (Query (ns, t, n)) = unwords ['@' : show ns, show t, show n]

equivQuery :: Query -> Query -> Bool
equivQuery (Query (_, qt1, qn1)) (Query (_, qt2, qn2)) = qt1 == qt2 && qn1 == qn2

data QueryState = QueryState
  { dnsData :: M.Map (Name String) (S.Set StandardRecord)
  , queried :: [Query]
  , addresses :: M.Map (Name String) [IP]
  , trustAnchors :: [RRSet String]
  , roots :: [Name String]
  , config :: QueryConfig
  , remainingQueries :: Int
  }

type RuleBase = Ex.ExceptT String (St.StateT QueryState IO)

type OnlineSolver a = Solver (Maybe T.Type) RuleBase a

getsState :: (QueryState -> a) -> RuleBase a
getsState = lift . St.gets

modState :: (QueryState -> QueryState) -> RuleBase ()
modState = lift . St.modify

runOnlineSolverT :: QueryState -> OnlineSolver a -> IO (Either String a, [Query])
runOnlineSolverT qst =
  fmap (fmap queried) . (`St.runStateT` qst) . Ex.runExceptT . fmap fst . runSLD

liftRuleBase :: RuleBase a -> OnlineSolver a
liftRuleBase = lift . lift . lift

getCfg :: OnlineSolver QueryConfig
getCfg = liftRuleBase $ getsState config

getRules :: OnlineSolver (S.Set (Rule (Maybe T.Type)))
getRules = liftRuleBase $ do
  dat <- getsState dnsData
  let apices = getApices dat
  let cfg = map (fmap compileWildcardsLiteral) $ splitZones apices $ map (fmap S.toList) $ M.toList dat
  rootNSes <- getsState roots
  let rootNSRules = map rootSRule rootNSes
  rootKeys <- getsState trustAnchors
  let rs = S.map (fmap (Just . fromType)) $ runFresh $ compileConfig rootNSes rootKeys cfg
  return $ foldr S.insert (S.filter (not . implicitKeyTrust) rs) rootNSRules
  where
    -- Live answers must validate against explicit trust anchors, rather than
    -- inheriting the static compiler's trust in starting servers' DNSKEYs.
    implicitKeyTrust (Rule _ (TFunctor "resolves" [_, t, _, _, _, v, a]) body) =
      t == dnskeyType && v == top && a == ad 1 && null body
    implicitKeyTrust _ = False

    getApices :: Foldable f => f (S.Set StandardRecord) -> Name String -> S.Set (Name String)
    getApices rsF n =
      let m = foldr getApex M.empty $ concatMap S.toList rsF
      in  M.findWithDefault S.empty n m

    getApex :: StandardRecord -> M.Map (Name String) (S.Set (Name String))
            -> M.Map (Name String) (S.Set (Name String))
    getApex (Named owner (rtype -> NS target)) =
      add (canonicalName target) (S.singleton (canonicalName owner))
    getApex _ = id

checkQueried :: Query -> OnlineSolver Bool
checkQueried q = liftRuleBase $ do
  b <- getsState (elem q . queried)
  unless b (modState (\qst -> qst { queried = q:queried qst }))
  return b

checkQueryLimit :: RuleBase Bool
checkQueryLimit = do
  b <- getsState ((> 0) . remainingQueries)
  when b (modState (\qst -> qst { remainingQueries = remainingQueries qst - 1 }))
  return b

putData :: Name String -> [StandardRecord] -> RuleBase ()
putData ns = mapM_ aux
  where
    aux :: StandardRecord -> RuleBase ()
    aux r@(Named n (rtype -> A ip)) = do
      modState (insertIP n (IPv4 ip))
      insertRecord r
    aux r@(Named n (rtype -> AAAA ip)) = do
      modState (insertIP n (IPv6 ip))
      insertRecord r
    aux r = insertRecord r

    insertRecord :: StandardRecord -> RuleBase ()
    insertRecord r = modState $ \qst -> qst { dnsData = add ns (S.singleton r) (dnsData qst) }

    insertIP :: Name String -> IP -> QueryState -> QueryState
    insertIP n ip qst =
      let addrs = addresses qst
      in qst { addresses = add n [ip] addrs }

getIPs :: Name String -> OnlineSolver [IP]
getIPs ns = liftRuleBase $ getsState (snub . M.findWithDefault [] ns . addresses)

goalToQuery :: Goal (Maybe T.Type) [Term] -> Maybe Query
goalToQuery (Goal [] _ _) = Nothing
goalToQuery (Goal (h:_) _ _) = case toView h of
  TVResolves r -> maybeExcept $ fmap Query $ (,,) <$> asName (resolvesNS r) <*> asTType (resolvesQType r) <*> asName (resolvesQName r)
  _ -> Nothing

dnsLookup :: Goal (Maybe T.Type) [Term] -> OnlineSolver Bool
dnsLookup = maybe (return False) dnsLookup' . goalToQuery
  where
    dnsLookup' q@(Query (ns, qt, qn)) = do
      seen <- checkQueried q
      if seen then return False else do
        ips <- getIPs ns
        cfg <- getCfg
        first ips (\ip -> dnsQuery cfg ns ip qt qn)

dnsQuery :: QueryConfig -> Name String -> IP -> T.Type -> Name String
            -> OnlineSolver Bool
dnsQuery cfg ns ip qt qn = liftRuleBase $ do
  withinLimit <- checkQueryLimit
  unless withinLimit $ Ex.throwE "DNS query limit exceeded"
  result <- lift $ lift $ queryServer cfg ip qt qn
  either (const $ return False)
    (\msg -> do
        putData ns $ mapMaybe fromResourceRecord (W.answer msg ++ W.authority msg ++ W.additional msg)
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

rootSRule :: Name String -> Rule (Maybe T.Type)
rootSRule ns =
  let q = TVar "Q"; n = TVar "N"; d = TVar "D"; v = TVar "V"; a = TVar "A"; z = TVar "Z"
  in Rule Nothing (queryT q n d v a)
       [resolves (nameTerm ns) q z n d v a, z \= referralZone]

sortFailures :: [Query] -> T.Type -> Name String -> [Goal (Maybe T.Type) [Term]]
             -> [Goal (Maybe T.Type) [Term]]
sortFailures qs qt qn = sortOn priority
  where
    priority failed = case goalToQuery failed of
      Nothing -> (3, True, 0, 0)
      Just q@(Query (_, t, n)) -> (group t, not (any (equivQuery q) qs), qnameDistance n, typePriority t)

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

runOnline :: T.Type -> Name String -> Goal (Maybe T.Type) [Term] -> Unifier
          -> OnlineSolver (Maybe Unifier)
runOnline qt qn g u = do
  rs <- getRules
  us <- sldSome (S.toList rs) g u
  failures <- clearFailures
  if null us then do
    qs <- liftRuleBase $ getsState queried
    join <$> firstP isJust (sortFailures qs qt qn (S.toList failures))
      (dnsLookup >=> (\new -> if new then runOnline qt qn g u else return Nothing))
  else return us

resolve :: [RRSet String] -> [(Name String, IP)] -> T.Type -> Name String -> IO (Maybe Term, [Query])
resolve = resolveWith defaultQueryConfig

resolveWith :: QueryConfig -> [RRSet String] -> [(Name String, IP)] -> T.Type -> Name String -> IO (Maybe Term, [Query])
resolveWith cfg anchors hints qt qn
  | queryPort cfg < 1 || queryPort cfg > 65535 = ioError (userError "DNS port must be between 1 and 65535")
  | queryTimeout cfg <= 0 || queryRetries cfg <= 0 = ioError (userError "DNS timeout and retries must be positive")
  | queryLimit cfg < 0 = ioError (userError "DNS query limit must be non-negative")
  | otherwise = do
      let addrs = M.fromListWith (++) $ map (bimap canonicalName (:[])) hints
          initial = QueryState M.empty [] addrs anchors
            (M.keys addrs) cfg (queryLimit cfg)
          q = queryT (ttypeTerm qt) (nameTerm (canonicalName qn)) (TVar "Result") top (TVar "Validated")
      (result, qstack) <- runOnlineSolverT initial (uncurry (runOnline qt qn) $ goal (resolutionLimits cfg) q)
      us <- either (const $ return Nothing) return result
      return ((`apply` q) <$> us, qstack)
