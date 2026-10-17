module Resolvable.QueryingSpec (tests) where

import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import DNS.Record (Type(A))
import qualified DNS.RecordType as T
import Parsing.Name (parseNameM)
import Parsing.ZoneFile (parseAddrs, parseDS)
import Resolvable.Prolog.Core
import Resolvable.Querying

cfg :: QueryConfig
cfg = defaultQueryConfig { queryLimit = 10, qtrace = const (return ()) }

liveQuery :: String -> Assertion
liveQuery domain = do
  hints <- parseAddrs "root-addresses"
  anchors <- parseDS "root-keys"
  qn <- parseNameM domain
  (answer, queries, _) <- maybe (fail "timeout") return =<< timeout 15000000 (resolveWith cfg anchors hints T.A qn)
  case answer of
    Just (TFunctor "query" [qt, name, records, _, _]) -> do
      qt @?= ttypeTerm T.A
      name @?= nameTerm qn
      maybe (assertFailure ("invalid answer RRSet: " ++ show records))
            (\addrs -> assertBool "expected a nonempty A RRSet" (not (null addrs) && all isAddress addrs))
            (collectList records)
    _ -> assertFailure ("no live answer; queries: " ++ show (reverse queries))
  where
    isAddress :: Term -> Bool
    isAddress (TComplex (A _)) = True
    isAddress _ = False

tests :: TestTree
tests = testGroup "Resolvable.Querying"
  [ testCase ("A " ++ domain) (liveQuery domain)
  | domain <- ["google.com.", "iana.org.", "dns-oarc.net."]
  ]
