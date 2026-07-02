{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.DisunificationSpec
  ( tests
  , q
  , qC ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Dependencies (ConstrainedQuery(..))
import Resolvable.Prolog.Core (Term(..), list, listWithTail)
import qualified Resolvable.ConstrainedUnification as CU
import Resolvable.Update.Disunification (withouts)
import Data.Char (isUpper)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.NegatedQuery"
    minusTests

minusTests :: [TestTree]
minusTests =
  map mkTestCase
    [ ( q ["com", "a"]
      , [q ["com", "X"]]
      , [] )
    , ( q ["com", "a"]
      , [q ["com", "a"]]
      , [] )
    , ( q ["com", "a"]
      , [q ["com", "b"]]
      , [ q ["com", "a"] ] )
    , ( q ["com", "a"]
      , [qC ["com", "X"] [("X", "a")]]
      , [ q ["com", "a"] ] )
    , ( q ["com", "a"]
      , [qC ["com", "X", "Y"] [("X", "*")]]
      , [ q ["com", "a"] ] )
    , ( q ["com", "a"]
      , [qTail ["com", "H"] "T" [("H", "*")]]
      , [] )
    , ( q ["com", "*"]
      , [qTail ["com", "H"] "T" [("H", "*")]]
      , [ q ["com", "*"] ] )
    , ( q ["com", "*"]
      , [qTail ["com", "H"] "T" [("H", "*")], q ["com", "*"]]
      , [] )
    , ( q ["com", "X"]
      , [q ["net", "a"]]
      , [ q ["com", "X"] ] )
    , ( q ["com", "X"]
      , [q ["com", "a"]]
      , [ qC ["com", "X"] [("X", "a")] ] )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "a"]]
      , [ qC ["com", "X"] [("X", "a")] ] )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "b"]]
      , [ qC ["com", "X"] [("X", "a"), ("X", "b")] ] )
    , ( qC ["com", "X"] [("X", "a")]
      , [q ["com", "Y"]]
      , [] )
    , ( qC ["com", "X"] [("X", "a")]
      , [qC ["com", "Y"] [("Y", "a")]]
      , [] )
    , ( qC ["com", "X"] [("X", "a")]
      , [qC ["com", "Y"] [("Y", "a"), ("Y", "b")]]
      , [ q ["com", "b"] ] )
    , ( q ["com", "X", "Y"]
      , [q ["com", "a", "b"]]
      , [ qC ["com", "X", "Y"] [("X", "a")], qC ["com", "X", "Y"] [("Y", "b")] ] )
    , ( q ["com", "X", "Y"]
      , [q ["com", "Z", "Z"]]
      , [ qC ["com", "X", "Y"] [("X", "Y")] ] ) ]
  where
    mkTestCase (q', n, expected) =
      testCase (show q' ++ " without " ++ intercalate " and " (map show n)) $
        withouts q' n @?= expected

q :: [String] -> ConstrainedQuery
q = (`qC` [])

qC :: [String] -> [(String, String)] -> ConstrainedQuery
qC (map fromString -> ts) (map (bimap fromString fromString) -> neqs) =
  CQuery (TFunctor "p" [list ts]) (fromMaybe (error "invalid test constraints") (CU.fromList neqs))

qTail :: [String] -> String -> [(String, String)] -> ConstrainedQuery
qTail (map fromString -> ts) (fromString -> tailVar) (map (bimap fromString fromString) -> neqs) =
  CQuery (TFunctor "p" [listWithTail ts tailVar]) (fromMaybe (error "invalid test constraints") (CU.fromList neqs))

fromString :: String -> Term
fromString "" = undefined
fromString s
  | all isUpper s = TVar s
  | otherwise = TAtom s
