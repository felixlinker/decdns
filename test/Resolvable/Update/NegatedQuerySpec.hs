{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.NegatedQuerySpec (tests) where

import Control.Monad (forM_)
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Resolvable.Dependencies (ConstrainedQuery(..))
import Resolvable.Prolog.Core (Term(..), list)
import Resolvable.Update.NegatedQuery (withouts)
import Data.Char (isUpper)
import Data.Bifunctor (Bifunctor(bimap))

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.NegatedQuery"
    [ testCase "minus" minusTests
    ]

minusTests :: Assertion
minusTests =
  forM_
    [ ( q ["com", "a"]
      , [q ["com", "X"]]
      , Nothing )
    , ( q ["com", "a"]
      , [q ["com", "b"]]
      , Just (q ["com", "a"]) )
    , ( q ["com", "a"]
      , [qC ["com", "X", "Y"] [("X", "*")]]
      , Just (q ["com", "a"]) )
    , ( q ["com", "X"]
      , [q ["net", "a"]]
      , Just (q ["com", "X"]))
    , ( q ["com", "X"]
      , [q ["com", "a"]]
      , Just (qC ["com", "X"] [("X", "a")]) )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "a"]]
      , Just (qC ["com", "X"] [("X", "a")]) )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "b"]]
      , Just (qC ["com", "X"] [("X", "a"), ("X", "b")]) )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "Y"]]
      , Nothing )
    , ( qC ["com", "X"] [("X", "a")]
      , [qC ["com", "Y"] [("Y", "a")]]
      , Just (qC ["com", "X"] [("X", "a")]) )
    , ( qC ["com", "X"] [("X", "a")]
      , [qC ["com", "Y"] [("Y", "a"), ("Y", "b")]]
      , Just (qC ["com", "X"] [("X", "a"), ("X", "b")]) ) ]
    $ \(q', n, expected) -> withouts q' n @?= expected

q :: [String] -> ConstrainedQuery
q = (`qC` [])

qC :: [String] -> [(String, String)] -> ConstrainedQuery
qC (map fromString -> ts) (map (bimap fromString fromString) -> neqs) = CQuery (list ts) (S.fromList neqs)

fromString :: String -> Term
fromString "" = undefined
fromString s
  | all isUpper s = TVar s
  | otherwise = TAtom s
