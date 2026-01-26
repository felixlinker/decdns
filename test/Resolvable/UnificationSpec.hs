module Resolvable.UnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck

import Resolvable.Prolog (Term(..))
import Resolvable.Unification (apply, unify)
import Util.Fresh (runFresh, fresh)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Unification"
    [ testProperty "apply unifier implies terms become equal" prop_unify_apply_agrees
    , testGroup
        "unit"
        [ testCase "occurs check rejects cyclic binding" occursCheckRejectsCycle
        , testCase "propagates substitutions through bindings" propagatesBindings
        , testCase "list tail unifies against longer list" listTailUnifies
        , testCase "fails on different arity" mismatchedArityFails
        , testCase "wildcard unifies without binding" wildcardKeepsIdentity
        ]
    ]

occursCheckRejectsCycle :: Assertion
occursCheckRejectsCycle =
  unify (TVar "X") (TOp "f" [TVar "X"]) @?= Nothing

propagatesBindings :: Assertion
propagatesBindings =
  case unify (TPred "p" [TVar "X", TVar "Y"]) (TPred "p" [TVar "Y", TAtom "a"]) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "Y") @?= TAtom "a"

listTailUnifies :: Assertion
listTailUnifies =
  case unify (TList [TVar "X"] (Just (TVar "T"))) (TList [TAtom "a", TAtom "b"] Nothing) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "T") @?= TList [TAtom "b"] Nothing

mismatchedArityFails :: Assertion
mismatchedArityFails =
  unify (TPred "p" [TAtom "a"]) (TPred "p" [TAtom "a", TAtom "b"]) @?= Nothing

wildcardKeepsIdentity :: Assertion
wildcardKeepsIdentity =
  case unify TWildcard (TAtom "a") of
    Nothing -> assertFailure "expected a unifier"
    Just u -> apply u (TVar "X") @?= TVar "X"

prop_unify_apply_agrees :: Property
prop_unify_apply_agrees =
  forAll (genTermNoWildcard names) $ \t1 ->
    forAll (genTermNoWildcard names) $ \t2 ->
      maybe (property True) (\u -> apply u t1 === apply u t2) (unify t1 t2)
  where
    names :: Gen String
    names = elements (runFresh $ mapM (const $ fresh "v") ([0..4] :: [Int]))

genTermNoWildcard :: Gen String -> Gen Term
genTermNoWildcard names = do
  n <- chooseInt (0, 6)
  go n
  where
    go :: Int -> Gen Term
    go n
      | n <= 0 = oneof  [ TVar <$> names
                        , TAtom <$> names
                        , TAtomQuoted <$> names
                        , TString <$> names
                        ]
      | otherwise =
        let rec = go (n - 1)
        in  frequency
            [ (4, go 0)
            , (2, TOp <$> names <*> genList rec)
            , (2, TPred <$> names <*> genList rec)
            , (2, TList <$> genList rec <*> genTail (n - 1)) ]

    genTail :: Int -> Gen (Maybe Term)
    genTail n = oneof [ pure Nothing, Just <$> genTailTerm n ]

    genTailTerm :: Int -> Gen Term
    genTailTerm n
      | n <= 0 = TVar <$> names
      | otherwise = oneof [ TVar <$> names, genTailList (n - 1) ]

    genTailList :: Int -> Gen Term
    genTailList n = TList <$> genList (go (n - 1)) <*> genTail (n - 1)

    genList :: Gen Term -> Gen [Term]
    genList g = do
      k <- chooseInt (0, 3)
      vectorOf k g
