module Resolvable.Constraint.StoreSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Prolog.Core
import Resolvable.Constraint.Store

tests :: TestTree
tests =
  testGroup
    "Resolvable.Constraint.Store"
    [ testCase "isName on free variable does not resolve" $
        checkIsName (TVar "X") @?= Nothing
    , testCase "isName accepts a concrete name" $
        checkIsName (list [TAtom "com", TAtom "example"]) @?= Just True
    , testCase "isName accepts the root name" $
        checkIsName (list []) @?= Just True
    , testCase "isName rejects an oversized name" $
        checkIsName (list (replicate 128 (TAtom "a"))) @?= Just False
    ]

checkIsName :: Term -> Maybe Bool
checkIsName n = solve . simplify =<< add empty (isName n)
