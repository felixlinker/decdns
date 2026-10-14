module Resolvable.QueryingSpec (tests) where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Monad (forever, void)
import Data.IORef
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Control.Exception (bracket, onException, try)
import qualified Data.ByteString.Char8 as BS
import Data.IP (IP(..))
import Data.Maybe (fromJust, isJust, isNothing, maybeToList)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Network.DNS as W
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import System.Directory (getTemporaryDirectory, removeFile, makeAbsolute)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.SEC.Crypto (DigestType(SHA256), AlgorithmType(ECDSAP256SHA256))
import DNS.SEC.Verification (keyTag, keyDigest)
import DNS.Response
import qualified DNS.ZoneFile as Z
import qualified DNS.RRSet as RRSet
import Parsing.Name (parseNameM)
import Parsing.ZoneFile (parseRecords, parseAddrs)
import Resolvable.Prolog.Core hiding (ds, keyTag)
import Resolvable.Querying
import Util.Bytes (serialize)

name' :: String -> Name String
name' = fromJust . parseNameM

server :: Name String
server = name' "ns.root."

-- Trust only the fixture KSK; derive its DS using the existing DNSSEC helpers.
fixtureAnchors :: IO [RRSet.RRSet String]
fixtureAnchors = do
  let path = "test/fixtures/querying/wire.zone"
  records <- readFile path >>= parseRecords path >>= either (fail . show) return
  let anchors = [Named owner (dat { rtype = DS $ DSData
                   (fromIntegral (keyTag k)) (_keyAlgorithm k) SHA256 (keyDigest root SHA256 k) })
                | Named owner dat@(RData _ _ (DNSKEY k)) <- records
                , owner == root, SecureEntryPoint `elem` _flags k]
  return $ maybe [] (Z.allRRSets . Z.fromRecordList) (NE.nonEmpty anchors)

mismatchAnchor :: RRSet.RRSet String -> RRSet.RRSet String
mismatchAnchor = fmap (fmap mismatch)
  where
    mismatch dat = case rtype dat of
      DS k -> dat { rtype = DS (k { _digest = BS.empty }) }
      _ -> dat

resolveFixture :: QueryConfig -> [(Name String, IP)] -> T.Type -> Name String -> IO (Maybe Term, [Query])
resolveFixture cfg hints qt qn = do
  anchors <- fixtureAnchors
  resolveWith cfg anchors (rootAddresses hints) qt qn

rootAddresses :: [(Name String, IP)] -> [RRSet.RRSet String]
rootAddresses hints =
  [Named root (NE.fromList [RData IN 60 (NS ns) | ns <- S.toList (S.fromList (map fst hints))])] ++
  map (\(owner, ip) -> Named owner (NE.singleton (RData IN 60 (case ip of
    IPv4 addr -> A addr
    IPv6 addr -> AAAA addr)))) hints

rootCache :: ZoneCache
rootCache = cacheRecords (S.singleton server) [Named root (RData IN 60 (NS server))] emptyZoneCache

-- CoreDNS is bound only to loopback, on an ephemeral unprivileged port.
startServer :: IO (QueryConfig, FilePath, ProcessHandle)
startServer = do
  port <- bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \sock -> do
    Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
    Socket.socketPort sock
  fixture <- makeAbsolute "test/fixtures/querying"
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "dns-regressor-Corefile"
  hPutStr handle $ unlines
    [ ".:" ++ show port ++ " {"
    , "  bind 127.0.0.1"
    , "  file " ++ fixture ++ "/test.zone ."
    , "}"
    , "child.test.:" ++ show port ++ " external.test.:" ++ show port ++ " {"
    , "  bind ::1"
    , "  file " ++ fixture ++ "/child.zone child.test."
    , "  file " ++ fixture ++ "/external.zone external.test."
    , "}"
    ]
  hClose handle
  (_, _, _, ph) <- createProcess (proc "coredns" ["-conf", path])
    { std_out = NoStream, std_err = Inherit }
  let cfg = defaultQueryConfig { queryPort = fromIntegral port, queryTimeout = 100000, queryRetries = 1 }
      resource = (cfg, path, ph)
      ready 0 = assertFailure "CoreDNS did not become ready"
      ready n = do
        status <- getProcessExitCode ph
        case status of
          Just code -> assertFailure ("CoreDNS exited: " ++ show code)
          Nothing -> do
            (answers, _) <- resolveFixture cfg [(server, IPv4 (read "127.0.0.1"))] T.A (name' "www.test.")
            if isNothing answers then threadDelay 50000 >> ready (n - 1) else return ()
  (ready (40 :: Int) >> return resource) `onException` stopServer resource

stopServer :: (QueryConfig, FilePath, ProcessHandle) -> IO ()
stopServer (_, path, ph) = terminateProcess ph >> waitForProcess ph >> removeFile path

results :: QueryConfig -> T.Type -> String -> IO (S.Set Term)
results cfg qt qn = S.map fst <$> validatedResults cfg qt qn

validatedResults :: QueryConfig -> T.Type -> String -> IO (S.Set (Term, Term))
validatedResults cfg qt qn = do
  result <- timeout 5000000 $ resolveFixture cfg [(server, IPv4 (read "127.0.0.1"))] qt (name' qn)
  case result of
    Nothing -> assertFailure "online resolution did not terminate" >> return S.empty
    Just (rs, _) -> return $ S.fromList [(d, a) | TFunctor "query" [_, _, d, v, a] <- maybeToList rs, v == top]

expect :: IO (QueryConfig, FilePath, ProcessHandle) -> T.Type -> String -> [Type] -> Assertion
expect resource qt qn ds = do
  (cfg, _, _) <- resource
  validated <- validatedResults cfg qt qn
  S.map snd validated @?= S.singleton (ad 1)
  let actual = S.map fst validated
  -- RRSet ordering is immaterial.
  S.map (S.fromList . fromJust . collectList) actual @?= S.singleton (S.fromList (map TComplex ds))

-- A counting loopback fixture also supports dropped packets for retry tests.
withWireServer :: (W.DNSMessage -> Maybe W.DNSMessage)
               -> (QueryConfig -> IO [W.DNSMessage] -> IO a) -> IO a
withWireServer respond = withWireServerM (return . respond)

withWireServerM :: (W.DNSMessage -> IO (Maybe W.DNSMessage))
                -> (QueryConfig -> IO [W.DNSMessage] -> IO a) -> IO a
withWireServerM = withWireFixture "wire"

withWireFixture :: String -> (W.DNSMessage -> IO (Maybe W.DNSMessage))
                -> (QueryConfig -> IO [W.DNSMessage] -> IO a) -> IO a
withWireFixture fixture = withWireFixtureModified fixture id

withWireFixtureModified :: String -> (W.DNSMessage -> W.DNSMessage)
                        -> (W.DNSMessage -> IO (Maybe W.DNSMessage))
                        -> (QueryConfig -> IO [W.DNSMessage] -> IO a) -> IO a
withWireFixtureModified fixture modify respond action = do
  signed <- loadWireFixture fixture
  withSignedWireServer (\msg -> fmap (fmap (modify . authenticate signed)) (respond msg)) action

withSignedWireServer :: (W.DNSMessage -> IO (Maybe W.DNSMessage))
                     -> (QueryConfig -> IO [W.DNSMessage] -> IO a) -> IO a
withSignedWireServer respond action = bracket acquire Socket.close $ \sock -> do
  port <- Socket.socketPort sock
  requests <- newIORef []
  let serve = forever $ do
        (packet, peer) <- SocketBS.recvFrom sock 4096
        case W.decode packet of
          Left _ -> return ()
          Right msg -> do
            atomicModifyIORef' requests (\qs -> (msg:qs, ()))
            reply <- respond msg
            case reply of
              Nothing -> return ()
              Just response -> void $ SocketBS.sendTo sock (W.encode response) peer
  bracket (forkIO serve) killThread $ \_ ->
    action (defaultQueryConfig { queryPort = fromIntegral port, queryTimeout = 100000, queryRetries = 1 })
      (readIORef requests)
  where
    acquire = do
      sock <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
      Socket.bind sock (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
        `onException` Socket.close sock
      return sock

-- Reuse the zone parser and record serializer to load real signatures into
-- the counting transport fixture. Supplying the root key with the response
-- keeps cache/budget tests focused on their original data lookups.
loadWireFixture :: String -> IO [W.ResourceRecord]
loadWireFixture fixture = do
  let path = "test/fixtures/querying/" ++ fixture ++ ".zone"
  parsed <- readFile path >>= parseRecords path
  records <- either (fail . show) return parsed
  let wire (Named owner dat) = W.ResourceRecord (BS.pack (show owner))
        (W.toTYPE (fromIntegral (T.typeToInt (toType (rtype dat))))) 1
        (fromIntegral (rttl dat)) (W.UnknownRData (serialize (rtype dat)))
  -- Decode the serialized RDATA using the DNS library's existing wire decoder.
  either (fail . show) (return . W.answer) $
    W.decode (W.encode W.defaultResponse { W.answer = map wire records })

authenticate :: [W.ResourceRecord] -> W.DNSMessage -> W.DNSMessage
authenticate fixture msg = msg
  { W.answer = withSignatures (W.answer msg)
  , W.authority = withSignatures authority
  , W.additional = W.additional msg ++ withSignatures keys
  }
  where
    keys = [r | r <- fixture, W.rrname r == BS.pack ".", W.rrtype r `elem` [W.NS, W.DNSKEY]]
    -- The signed wire fixture is served directly by the root.
    authority = concatMap (\r -> if W.rrtype r == W.SOA
      then [soa | soa <- fixture, W.rrtype soa == W.SOA] else [r]) (W.authority msg)
      ++ [r | r <- fixture, W.rrtype r == W.NSEC3 || (W.rrtype r == W.NSEC &&
            any (\ns -> W.rrtype ns == W.NS && W.rrname ns == W.rrname r) (W.authority msg))]
    withSignatures rs = rs ++ [r | r <- fixture, W.RD_RRSIG sig <- [W.rdata r],
      any (\dataRR -> W.rrname dataRR == W.rrname r && W.rrtype dataRR == W.rrsigType sig) rs]

aliasResponse :: Bool -> W.DNSMessage -> Maybe W.DNSMessage
aliasResponse positive msg = Just W.defaultResponse
  { W.header = (W.header msg)
      { W.flags = (W.flags (W.header W.defaultResponse))
          { W.authAnswer = True, W.rcode = if isAlias || positive then W.NoErr else W.NameErr }
      }
  , W.question = W.question msg
  , W.answer = if isAlias
      then [W.ResourceRecord alias W.CNAME 1 60 (W.RD_CNAME target)]
      else [W.ResourceRecord target W.A 1 60 (W.RD_A (read "192.0.2.50"))
           | positive, any (\q -> W.qname q == target && W.qtype q == W.A) (W.question msg)]
  , W.authority = [W.ResourceRecord (BS.pack "test.") W.SOA 1 60
      (W.RD_SOA (BS.pack "ns.test.") (BS.pack "hostmaster.test.") 1 60 60 60 60)]
  }
  where
    target = BS.pack "target.test."
    alias = BS.pack "alias.test."
    isAlias = any ((== alias) . W.qname) (W.question msg)

-- Both starting servers may be explored. Cached responses should prevent
-- repeated wire lookups at the same server, regardless of search order.
cacheTest :: Bool -> Assertion
cacheTest positive = withWireServer (aliasResponse positive) $ \cfg requests -> do
  let hints = [(name' n, IPv4 (read "127.0.0.1")) | n <- ["ns.root.", "other.test."]]
  answer <- timeout 5000000 (resolveFixture cfg hints T.A (name' "alias.test."))
  case answer of
    Nothing -> assertFailure "cached resolution did not terminate"
    Just (rs, _) -> isNothing rs @?= not positive
  qs <- requests
  assertBool "all queries must clear RD" (all (not . W.recDesired . W.flags . W.header) qs)
  let counts = M.fromListWith (+)
        [((W.qname q, W.qtype q), 1 :: Int) | msg <- qs, q <- W.question msg]
  assertBool "cached questions must not be repeated at a starting server"
    (all (<= length hints) (M.elems counts))

-- An alias and its target answer coexist in a single response.
completeResponse :: W.DNSMessage -> Maybe W.DNSMessage
completeResponse msg = do
  response <- aliasResponse True msg
  return response { W.answer = W.answer response ++
    [W.ResourceRecord (BS.pack "target.test.") W.A 1 60 (W.RD_A (read "192.0.2.51"))] }

-- All response sections populate the transport address cache. The NS referral
-- and its in-bailiwick addresses must also compile as referral/glue rules.
-- The working address is mixed with an unreachable one to exercise both paths.
addressCacheTest :: String -> ([W.ResourceRecord] -> W.DNSMessage -> W.DNSMessage) -> Assertion
addressCacheTest fixture = delegationTest fixture id True

delegationTest :: String -> (W.DNSMessage -> W.DNSMessage) -> Bool
               -> ([W.ResourceRecord] -> W.DNSMessage -> W.DNSMessage) -> Assertion
delegationTest fixture modify succeeds addAddresses = do
  count <- newIORef (0 :: Int)
  let respond msg = do
        n <- atomicModifyIORef' count (\n -> (n + 1, n))
        let questions = [(W.qname q, W.qtype q) | q <- W.question msg]
        return $ Just $ if questions /= [(BS.pack "www.child.test.", W.A)]
          then (answer msg) { W.answer = [] }
          else if n == 0 then referral msg else answer msg
  withWireFixtureModified fixture modify respond $ \cfg _ -> do
    -- Missing denial proofs exhaust more discovery alternatives than the
    -- deliberately small default live-query budget permits.
    actual <- validatedResults (cfg { queryLimit = 100 }) T.A "www.child.test."
    actual @?= if succeeds
      then S.singleton (list [TComplex (A (read "192.0.2.30"))], ad 0) else S.empty
  where
    ns = BS.pack "ns.child.test."
    address ip = W.ResourceRecord ns W.A 1 60 (W.RD_A (read ip))
    referral msg = addAddresses [address "127.0.0.1"] W.defaultResponse
      { W.header = (W.header msg) { W.flags = (W.flags (W.header W.defaultResponse)) { W.authAnswer = False } }
      , W.question = W.question msg
      , W.authority = [W.ResourceRecord (BS.pack "child.test.") W.NS 1 60 (W.RD_NS ns)]
      , W.additional = [address "127.0.0.2"]
      }
    answer msg = W.defaultResponse
      { W.header = (W.header msg) { W.flags = (W.flags (W.header W.defaultResponse)) { W.authAnswer = True } }
      , W.question = W.question msg
      , W.answer = [W.ResourceRecord (BS.pack "www.child.test.") W.A 1 60 (W.RD_A (read "192.0.2.30"))]
      }

limitTest :: Int -> Int -> Bool -> Assertion
limitTest budget retries reply =
  withWireServer (if reply then aliasResponse True else const Nothing) $ \cfg requests -> do
    let hints = [(server, IPv4 (read "127.0.0.1")), (name' "other.test.", IPv4 (read "127.0.0.1"))]
    result <- timeout 5000000 $ resolveFixture
      (cfg { queryLimit = budget, queryRetries = retries }) hints T.A (name' "alias.test.")
    case result of
      Just (answers, _) -> answers @?= Nothing
      Nothing -> assertFailure "budget-limited resolution did not terminate"
    qs <- requests
    if budget == 0
      then assertBool "zero budget must prevent network requests" (null qs)
      else assertBool "resolution must try a lookup before exhausting the budget" (not (null qs))

tamper :: W.ResourceRecord -> W.ResourceRecord
tamper r = case W.rdata r of
  W.RD_A _ -> r { W.rdata = W.RD_A (read "192.0.2.99") }
  _ -> r

tests :: TestTree
tests = testGroup "Resolvable.Querying"
  [ testGroup "root DS trust anchors"
      [ testCase "anchors compile to an authentic root DS RRSet without lookups" $ do
          anchors <- fixtureAnchors
          (answers, queries) <- resolveWith (defaultQueryConfig { queryLimit = 0 })
            anchors (rootAddresses [(server, IPv4 (read "127.0.0.1"))]) T.DS root
          answers @?= Just (TFunctor "query" [dsType, nameTerm root,
            rrsetTerm (concatMap (NE.toList . value) anchors), top, ad 1])
          queries @?= []
      , testCase "a valid anchor among multiple anchors validates answers" $
          withWireServer (aliasResponse True) $ \cfg _ -> do
            anchors <- fixtureAnchors
            let wrong = map mismatchAnchor anchors
            (answers, _) <- resolveWith cfg (wrong ++ anchors)
              (rootAddresses [(server, IPv4 (read "127.0.0.1"))]) T.A (name' "alias.test.")
            assertBool "expected a validated answer" (isJust answers)
      , testCase "missing or mismatching anchors cannot validate root DNSKEYs" $
          withWireServer (aliasResponse True) $ \cfg _ -> do
            anchors <- fixtureAnchors
            mapM_ (\keys -> do
              (answers, _) <- resolveWith (cfg { queryLimit = 100 }) keys
                (rootAddresses [(server, IPv4 (read "127.0.0.1"))]) T.A (name' "alias.test.")
              answers @?= Nothing) [[], map mismatchAnchor anchors]
      , testCase "root DNSKEYs require a valid self-signature" $
          withWireFixtureModified "wire"
            (\msg -> msg { W.additional = filter (\r -> case W.rdata r of
              W.RD_RRSIG sig -> W.rrsigType sig /= W.DNSKEY
              _ -> True) (W.additional msg) })
            (return . aliasResponse True) $ \cfg _ ->
              results (cfg { queryLimit = 100 }) T.A "alias.test." >>= (@?= S.empty)
      ]
  , testCase "parent keys are fetched before retrying child keys at another server" $ do
      parent <- loadWireFixture "test"
      child <- loadWireFixture "child"
      firstAnswer <- newIORef True
      let rootKeys r = W.rrtype r == W.DNSKEY || case W.rdata r of
            W.RD_RRSIG sig -> W.rrsigType sig == W.DNSKEY
            _ -> False
          ns owner target = W.ResourceRecord (BS.pack owner) W.NS 1 60 (W.RD_NS (BS.pack target))
          address target = W.ResourceRecord (BS.pack target) W.A 1 60 (W.RD_A (read "127.0.0.1"))
          -- Both child servers answer with the same keys. Their validation
          -- still needs the root DNSKEY RRSet, which is deliberately withheld.
          parentData = filter (not . rootKeys) parent ++
            [ns "child.test." "other.child.test.", address "ns.child.test.", address "other.child.test."]
          childData = child ++ [ns "child.test." "other.child.test."]
          respond msg = do
            first <- atomicModifyIORef' firstAnswer (\b -> (False, b))
            return $ Just W.defaultResponse
              { W.header = (W.header msg) { W.flags = W.flags (W.header W.defaultResponse) }
              , W.question = W.question msg
              , W.answer = if any ((== BS.pack ".") . W.qname) (W.question msg)
                  then parent else if any ((== W.DNSKEY) . W.qtype) (W.question msg)
                    then childData else if first || all ((/= BS.pack "www.child.test.") . W.qname) (W.question msg)
                      then parentData else filter (not . rootKeys) childData
              }
      withSignedWireServer respond $ \cfg requests -> do
        actual <- validatedResults cfg T.A "www.child.test."
        qs <- requests
        actual @?= S.singleton (list [TComplex (A (read "192.0.2.30"))], ad 1)
        let keyQuestions = [W.qname q | msg <- reverse qs, q <- W.question msg, W.qtype q == W.DNSKEY]
        keyQuestions @?= map BS.pack ["child.test.", "."]
  , testCase "a large delegation reuses cached addresses outside its child zone" $ do
      firstAnswer <- newIORef True
      let childName = BS.pack "child.test."
          target = BS.pack "www.child.test."
          servers = [BS.pack ("ns" ++ show i ++ ".other.test.") | i <- [1..13 :: Int]]
          respond msg = do
            first <- atomicModifyIORef' firstAnswer (\b -> (False, b))
            return $ Just W.defaultResponse
              { W.header = (W.header msg) { W.flags = W.flags (W.header W.defaultResponse) }
              , W.question = W.question msg
              , W.answer = [W.ResourceRecord target W.A 1 60 (W.RD_A (read "192.0.2.30")) | not first]
              , W.authority = [W.ResourceRecord childName W.NS 1 60 (W.RD_NS ns) | first, ns <- servers]
              , W.additional = [W.ResourceRecord ns W.A 1 60 (W.RD_A (read "127.0.0.1")) | first, ns <- servers]
              }
      withWireFixture "wire" respond $ \cfg requests -> do
        actual <- validatedResults (cfg { queryLimit = 3 }) T.A "www.child.test."
        qs <- requests
        actual @?= S.singleton (list [TComplex (A (read "192.0.2.30"))], ad 0)
        [(W.qname q, W.qtype q) | msg <- reverse qs, q <- W.question msg]
          @?= [(target, W.A), (BS.pack ".", W.DNSKEY), (target, W.A)]
  , testGroup "addresses cached from every response section"
      [ testCase "answer" $ addressCacheTest "wire" (\rs msg -> msg { W.answer = rs })
      , testCase "authority" $ addressCacheTest "wire" (\rs msg -> msg { W.authority = W.authority msg ++ rs })
      , testCase "additional" $ addressCacheTest "wire" (\rs msg -> msg { W.additional = rs ++ W.additional msg })
      ]
  , testCase "unsigned delegation proved by NSEC3" $
      addressCacheTest "wire-nsec3" (\rs msg -> msg { W.additional = rs ++ W.additional msg })
  , testCase "unsigned delegation proved by NSEC3 opt-out" $
      addressCacheTest "wire-optout" (\rs msg -> msg { W.additional = rs ++ W.additional msg })
  , testGroup "unsigned delegation requires authenticated denial"
      [ testCase (fixture ++ " " ++ label) $
          delegationTest fixture (\msg -> msg { W.authority = filter keep (W.authority msg) }) False
            (\rs msg -> msg { W.additional = rs ++ W.additional msg })
      | fixture <- ["wire", "wire-nsec3", "wire-optout"]
      , (label, keep) <-
          [ ("missing proof", \r -> W.rrtype r `notElem` [W.NSEC, W.NSEC3])
          , ("missing proof signature", \r -> case W.rdata r of
                W.RD_RRSIG sig -> W.rrsigType sig `notElem` [W.NSEC, W.NSEC3]
                _ -> True)
          ]
      ]
  , testGroup "signed answers require valid signatures"
      [ testCase label $
          withWireFixtureModified "wire" modify (return . aliasResponse True) $ \cfg _ ->
            results cfg T.A "alias.test." >>= (@?= S.empty)
      | (label, modify) <-
          [ ("missing answer signature", \msg -> msg { W.answer = filter ((/= W.RRSIG) . W.rrtype) (W.answer msg) })
          , ("tampered answer", \msg -> msg { W.answer = map tamper (W.answer msg) })
          ]
      ]
  , testCase "SOA-free answers accumulate with DNSSEC discovery" $
      withWireServer (fmap (\msg -> msg { W.authority = [] }) . aliasResponse True) $ \cfg _ -> do
        actual <- results cfg T.A "alias.test."
        actual @?= S.singleton (list [TComplex (A (read "192.0.2.50"))])
  , testCase "signed answers accumulate across a later SOA response" $
      withWireServer (\request -> do
        response <- aliasResponse True request
        return $ if any ((== BS.pack "alias.test.") . W.qname) (W.question request)
          then response { W.authority = [] } else response) $ \cfg requests -> do
        actual <- results cfg T.A "alias.test."
        actual @?= S.singleton (list [TComplex (A (read "192.0.2.50"))])
        qs <- requests
        let questions = [(W.qname q, W.qtype q) | msg <- qs, q <- W.question msg]
        length questions @?= S.size (S.fromList questions)
  , testGroup "zone cache by origin and apex"
      [ testCase "starts empty" $ cachedZones emptyZoneCache @?= []
      , testCase "address-only root hints are rejected" $ do
          let hints = [Named server (NE.singleton (RData IN 60 (A (read "127.0.0.1"))))]
          result <- try (resolveWith (defaultQueryConfig { queryLimit = 0 }) [] hints T.A (name' "google.com."))
            :: IO (Either IOError (Maybe Term, [Query]))
          case result of
            Left _ -> return ()
            Right _ -> assertFailure "missing root NS records must be reported"
      , testCase "addresses do not discover zones" $ do
          let records = concatMap (NE.toList . RRSet.toRecords) $
                rootAddresses [(server, IPv4 (read "127.0.0.1"))]
              zones = cachedZones (cacheRecords (S.singleton server) records emptyZoneCache)
          map (toStringName . Z.zoneName) zones @?= [root]
          sort (map (mapName toStringName) (concatMap Z.allRecords zones)) @?= sort records
      , testCase "root referrals use the root origin despite discovering a closer child zone" $ do
          let child = name' "a.gtld-servers.net."
              ns = Named (name' "com.") (RData IN 60 (NS child))
              ds = Named (name' "com.") (RData IN 60 (DS (DSData 1 ECDSAP256SHA256 SHA256 BS.empty)))
              glue = Named child (RData IN 60 (A (read "192.5.6.30")))
              zones = cachedZones (cacheRecords (S.singleton server) [ns, ds, glue] rootCache)
              at apex = [mapName toStringName rr | z <- zones, toStringName (Z.zoneName z) == apex,
                                                  rr <- Z.allRecords z]
          sort (map (toStringName . Z.zoneName) zones) @?= [root, name' "com."]
          sort (at root) @?= sort (Named root (RData IN 60 (NS server)) : [ns, ds, glue])
          at (name' "com.") @?= [ns]
      , testCase "additional records outside an origin's zones do not create new zones" $ do
          let child = name' "ns.child.test."
              ns = Named (name' "child.test.") (RData IN 60 (NS child))
              answer = Named (name' "www.child.test.") (RData IN 60 (A (read "192.0.2.50")))
              unrelated = Named (name' "other.test.") (RData IN 60 (A (read "192.0.2.51")))
              delegated = cacheRecords (S.singleton server) [ns] rootCache
              zones = cachedZones (cacheRecords (S.singleton child) [answer, unrelated] delegated)
              records = map (mapName toStringName) (concatMap Z.allRecords zones)
          assertBool "the child answer is retained" (answer `elem` records)
          assertBool "unrelated additional data is ignored" (unrelated `notElem` records)
          sort (map (toStringName . Z.zoneName) zones) @?= sort [root, name' "child.test."]
      , testCase "repeated responses deduplicate complete RRSets" $ do
          let records = [Named (name' "www.test.") (RData IN 60 (A (read ip))) |
                         ip <- ["192.0.2.50", "192.0.2.51"]]
              cache = cacheRecords (S.singleton server) records rootCache
              repeated = cacheRecords (S.singleton server) (reverse records) cache
          cachedZones cache @?= cachedZones repeated
      ]
  , testCase "successful resolution with multiple starting servers" $ cacheTest True
  , testCase "a response containing a complete proof resolves the query" $
      withWireFixture "wire-complete" (return . completeResponse) $ \cfg _ -> do
        let hints = [(name' n, IPv4 (read "127.0.0.1")) | n <- ["ns.root.", "other.test."]]
        answer <- timeout 5000000 (resolveFixture cfg hints T.A (name' "alias.test."))
        case answer of
          Just (rs, _) -> [d | TFunctor "query" [_, _, d, _, _] <- maybeToList rs]
            @?= [list [TComplex (A (read "192.0.2.51"))]]
          Nothing -> assertFailure "resolution did not terminate"
  , testCase "starting hints are replaced by the advertised root NS set" $
      withWireFixtureModified "wire-complete"
        (\msg -> msg { W.additional = W.ResourceRecord (BS.pack ".") W.NS 1 60
          (W.RD_NS (BS.pack "other.root.")) : W.additional msg })
        (return . completeResponse) $ \cfg _ -> do
          actual <- results cfg T.A "alias.test."
          actual @?= S.singleton (list [TComplex (A (read "192.0.2.51"))])
  , testCase "missing apex NS records use the complete starting hint set" $
      withWireFixtureModified "wire-complete"
        (\msg -> msg { W.additional = filter ((/= W.NS) . W.rrtype) (W.additional msg) })
        (return . completeResponse) $ \cfg _ -> do
          let hints = [(name' n, IPv4 (read "127.0.0.1")) | n <- ["ns.root.", "other.root."]]
          (answer, _) <- resolveFixture cfg hints T.A (name' "alias.test.")
          [d | TFunctor "query" [_, _, d, _, _] <- maybeToList answer]
            @?= [list [TComplex (A (read "192.0.2.51"))]]
  , testCase "non-recursive queries and negative response reuse" $ cacheTest False
  , testCase "zero budget sends no queries" $ limitTest 0 1 True
  , testCase "budget is shared across alias lookups" $ limitTest 1 1 True
  , testCase "resolution limits apply to every online pass" $
      withWireServer (aliasResponse True) $ \cfg _ -> do
        actual <- results (cfg { _resolutionLimits = M.singleton T.CNAME 0 }) T.A "alias.test."
        actual @?= S.empty
  , testCase "repeated cached addresses do not repeat wire lookups" $
      let address = W.ResourceRecord (BS.pack "ns.root.") W.A 1 60 (W.RD_A (read "127.0.0.1"))
      in withWireServer (fmap (\msg -> msg { W.additional = replicate 3 address }) . aliasResponse True) $
        \cfg requests -> do
          actual <- results cfg T.A "alias.test."
          actual @?= S.singleton (list [TComplex (A (read "192.0.2.50"))])
          qs <- requests
          let questions = [(W.qname q, W.qtype q) | msg <- qs, q <- W.question msg]
          length questions @?= S.size (S.fromList questions)
  , testCase "budget is shared across servers even with retries" $ limitTest 1 2 False
  , testCase "root address files supply complete IPv4 and IPv6 RRSets" $
      withWireFixture "wire-complete" (return . completeResponse) $ \cfg _ -> do
        anchors <- fixtureAnchors
        tmp <- getTemporaryDirectory
        bracket (openTempFile tmp "dns-regressor-root-addresses")
          (\(path, _) -> removeFile path) $ \(path, handle) -> do
            hPutStr handle $ unlines
              [ "NS.ROOT. 60 IN A 127.0.0.2"
              , "NS.ROOT. 60 IN A 127.0.0.1"
              , "NS.ROOT. 60 IN AAAA ::1"
              , ". 60 IN NS NS.ROOT."
              ]
            hClose handle
            hints <- parseAddrs path
            sort (map RRSet.rrsetType hints) @?= sort [T.NS, T.A, T.AAAA]
            (answers, _) <- resolveWith cfg anchors hints T.A (name' "alias.test.")
            [d | TFunctor "query" [_, _, d, _, _] <- maybeToList answers]
              @?= [list [TComplex (A (read "192.0.2.51"))]]
  , testCase "retries do not reserve lookup budget" $
      withWireFixture "wire-complete" (return . completeResponse) $ \cfg _ -> do
        actual <- results (cfg { queryLimit = 1, queryRetries = 3 }) T.A "alias.test."
        actual @?= S.singleton (list [TComplex (A (read "192.0.2.51"))])
  , testCase "wire conversion preserves TTL and complete IPv4 data" $
      fromResourceRecord (W.ResourceRecord (BS.pack "www.test.") W.A 1 60 (W.RD_A (read "192.0.2.10")))
        @?= Just (Named (name' "www.test.") (RData IN 60 (A (read "192.0.2.10"))))
  , testCase "unsupported opaque wire data is ignored" $ do
      let dat = ZONEMD 1234 1 1 (BS.pack "digest")
          rr = W.ResourceRecord (BS.pack "test.") (W.toTYPE 63) 1 60 (W.UnknownRData (serialize dat))
      fromResourceRecord rr @?= Nothing
  , testCase "SOA mailbox conversion survives a wire roundtrip" $ do
      let rr = W.ResourceRecord (BS.pack "test.") W.SOA 1 60
            (W.RD_SOA (BS.pack "ns.test.") (BS.pack "hostmaster@test.") 1 60 60 60 60)
      case W.decode (W.encode W.defaultResponse { W.answer = [rr] }) of
        Left err -> assertFailure (show err)
        Right msg -> mapM fromResourceRecord (W.answer msg) @?= Just
          [Named (name' "test.") (RData IN 60 (SOA (name' "ns.test.") (name' "hostmaster.test.") 1 60 60 60 60))]
  , withResource startServer stopServer $ \resource -> testGroup "local CoreDNS"
      [ testCase "complete A RRSet" $ expect resource T.A "www.test." [A (read "192.0.2.10"), A (read "192.0.2.11")]
      , testCase "AAAA" $ expect resource T.AAAA "www.test." [AAAA (read "2001:db8::10")]
      , testCase "CNAME chain" $ expect resource T.A "chain.test." [A (read "192.0.2.10"), A (read "192.0.2.11")]
      , testCase "CNAME itself" $ expect resource T.CNAME "alias.test." [CNAME (name' "www.test.")]
      , testCase "wildcard answer" $ expect resource T.A "hello.wild.test." [A (read "192.0.2.20")]
      -- The parent supplies ns.child.test. AAAA ::1 as glue. The child is
      -- reachable only over IPv6, but the requested answer is still an A RRSet.
      , testCase "signed delegation with IPv6 glue" $ expect resource T.A "www.child.test." [A (read "192.0.2.30")]
      , testCase "signed delegation without in-bailiwick glue" $ expect resource T.A "www.external.test." [A (read "192.0.2.40")]
      , testCase "NXDOMAIN" $ do
          (cfg, _, _) <- resource
          results cfg T.A "missing.test." >>= (@?= S.empty)
      , testCase "NODATA" $ do
          (cfg, _, _) <- resource
          results cfg T.MX "www.test." >>= (@?= S.empty)
      ]
  ]
