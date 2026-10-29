module Resolvable.Constraint.StoreSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Prolog.Core
import Resolvable.Constraint.Store
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
        map toBody (simplify constraints) @?= [[isName (TVar "X")]]
        map toBody (runFresh (evalCached (simplifyCached constraints)))
          @?= [[isName (TVar "X")]]
    , testCase "a false constraint rejects a conjunction containing a true constraint" $ do
        let constraints = CStore []
              [isName (list (replicate 128 (TAtom "a"))), isName (list [])]
        solve (simplify constraints) @?= Just False
    , testCase "negating true yields false" $ do
        let constraints = CStore [] [nott (isName (list []))]
        solve (simplify constraints) @?= Just False
    , testCase "negation supports syntactic contradictions" $ do
        let constraints = CStore [] [nott (isName (TVar "X")), isName (TVar "X")]
        solve (simplify constraints) @?= Just False
    , testCase "negation supports not solvable constraints" $ do
        let constraints = CStore [] [nott (isName (TVar "X"))]
        solve (simplify constraints) @?= Nothing
    , testCase "disjunction preservation" $ do
        let constraints = CStore [(list [TVar "X", TVar "Y"], list [TVar "A", TVar "B"])] [isName (TVar "X")]
        simplify constraints @?= [ CStore [(TVar "X", TVar "A")] [isName (TVar "X")]
                                 , CStore [(TVar "Y", TVar "B")] [isName (TVar "X")] ]
    ]

checkIsName :: Term -> Maybe Bool
checkIsName n = solve . simplify =<< add empty (isName n)
