{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.DisunificationSpec
  ( tests
  , q
  , qC ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Consequences
import qualified Resolvable.Constraint.Store as Store
import Resolvable.Prolog.Core
import Resolvable.Update.Disunification (withouts)

import Control.Monad (foldM)
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.Disunification"
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
      , [qC ["com", "X"] ["X" != "a"]]
      , [ q ["com", "a"] ] )
    , ( q ["com", "a"]
      , [qC ["com", "X", "Y"] ["X" != "*"]]
      , [ q ["com", "a"] ] )
    , ( q ["com", "a"]
      , [qTail ["com", "H"] "T" ["H" != "*"]]
      , [] )
    , ( q ["com", "*"]
      , [qTail ["com", "H"] "T" ["H" != "*"]]
      , [ q ["com", "*"] ] )
    , ( q ["com", "*"]
      , [qTail ["com", "H"] "T" ["H" != "*"], q ["com", "*"]]
      , [] )
    , ( q ["com", "X"]
      , [q ["net", "a"]]
      , [ q ["com", "X"] ] )
    , ( q ["com", "X"]
      , [q ["com", "a"]]
      , [ qC ["com", "V0"] ["V0" != "a"] ] )
    , ( qC ["com", "X"] ["X" != "a"]
      , [q ["com", "a"]]
      , [ qC ["com", "X"] ["X" != "a"] ] )
    , ( qC ["com", "X"] ["X" != "a"]
      , [q ["com", "b"]]
      , [ qC ["com", "V0"] ["V0" != "a", "V0" != "b"] ] )
    , ( qC ["com", "X"] ["X" != "a"]
      , [q ["com", "Y"]]
      , [] )
    , ( qC ["com", "X"] ["X" != "a"]
      , [qC ["com", "Y"] ["Y" != "a"]]
      , [] )
    , ( qC ["com", "X"] ["X" != "a"]
      , [qC ["com", "Y"] ["Y" != "a", "Y" != "b"]]
      , [ q ["com", "b"] ] )
    , ( q ["com", "X", "Y"]
      , [q ["com", "a", "b"]]
      , [ qC ["com", "V0", "V1"] ["V0" != "a"], qC ["com", "V0", "V1"] ["V1" != "b"] ] )
    , ( q ["com", "X", "Y"]
      , [q ["com", "Z", "Z"]]
      , [ qC ["com", "X", "Y"] ["X" != "Y"] ] )
    , ( q ["com", "X", "Y"]
      , [ qC ["com", "a", "Z"] ["Z" != "b"] ]
      , [ qC ["com", "X", "Y"] ["X" != "a"], q ["com", "a", "b"] ] )
    , ( q ["com", "X"]
      , [qC ["com", "Y"] [isName (TVar "Y")]]
      , [ qC ["com", "Y"] [nott (isName (TVar "Y"))] ] )
    , ( qC ["com", "X"] [isName (TVar "X")]
      , [qC ["com", "Y"] [isName (TVar "Y")]]
      , [] )
    -- , ( q [ "X", "X" ]
    --   , [ qF [ f (fromString "A") (fromString "B"), f (fromString "C") (fromString "D") ] ]
    --   , [] )
    -- , ( q [ "X", "Y", "Z" ]
    --   , [ q [ "A", "A", "A" ] ]
    --   , [ qC [ "X", "Y", "Z" ] [ "X" != "Y" ]
    --     , qC [ "X", "Y", "Z" ] [ "X" != "Z" ]
    --     , qC [ "X", "Y", "Z" ] [ "Y" != "Z" ] ] )
    ]
  where
    mkTestCase (q', n, expected) =
      testCase (show q' ++ " without " ++ intercalate " and " (map show n)) $
        withouts q' n @?= expected

f :: Term -> Term -> Term
f a b = TFunctor "f" [ a, b ]

q :: [String] -> Fact
q = (`qC` [])

labelList :: [String] -> Term
labelList = list . map fromString

qF :: [Term] -> Fact
qF ts = Fact (TFunctor "p" [ list ts ]) Store.empty

qC :: [String] -> [Term] -> Fact
qC (labelList -> ls) neqs =
  Fact (TFunctor "p" [ls]) (store neqs)

qTail :: [String] -> String -> [Term] -> Fact
qTail (map fromString -> ts) (fromString -> tailVar) neqs =
  Fact (TFunctor "p" [listWithTail ts tailVar]) (store neqs)

store :: [Term] -> Store.ConstraintStore
store = fromMaybe (error "invalid test constraints") . foldM Store.add Store.empty

(!=) :: String -> String -> Term
a != b = fromString a \= fromString b

fromString :: String -> Term
fromString "" = undefined
fromString s@(h:_)
  | isUpper h = TVar s
  | otherwise = TAtom s
