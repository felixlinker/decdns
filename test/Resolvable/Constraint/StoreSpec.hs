module Resolvable.Constraint.StoreSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Resolvable.Prolog.Core
import Resolvable.Constraint.Store

tests :: TestTree
tests =
  testGroup
    "Resolable.Constraint.Store"
    [ testCase "isName on free variable does not resolve" isNameFree ]

isNameFree :: Assertion
isNameFree = (solve . simplify =<< add empty (isName (TVar "X"))) @?= Nothing
