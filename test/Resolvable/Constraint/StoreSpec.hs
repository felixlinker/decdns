module Resolvable.Constraint.StoreSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Prolog.Core
import Resolvable.Constraint.Store
import Resolvable.Constraint.Solving (getSolved)
import Util.Fresh (runFresh)

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
    , testCase "simplification retains unresolved constraints beside true constraints" $ do
        let constraints = CStore [] [isName (TVar "X"), isName (list [])]
        fmap toBody (getSolved (simplify constraints)) @?= Just [isName (TVar "X")]
        fmap toBody (getSolved (runFresh (evalCached (simplifyCached constraints))))
          @?= Just [isName (TVar "X")]
    , testCase "a false constraint rejects a conjunction containing a true constraint" $ do
        let constraints = CStore []
              [isName (list (replicate 128 (TAtom "a"))), isName (list [])]
        solve (simplify constraints) @?= Just False
        solve (runFresh (evalCached (simplifyCached constraints))) @?= Just False
    ]

checkIsName :: Term -> Maybe Bool
checkIsName n = solve . simplify =<< add empty (isName n)
