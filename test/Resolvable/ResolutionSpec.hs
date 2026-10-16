{-# LANGUAGE TupleSections #-}
module Resolvable.ResolutionSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad (join)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (maybeToList)
import Data.List (uncons)
import Control.Exception (evaluate)
import System.Timeout (timeout)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Prolog.Core
import Resolvable.Resolution
import Resolvable.Unification (Unifier, apply)

type StubLimits = M.Map () Int

type SolverF m a = [Rule ()] -> Goal StubLimits [Term] -> Unifier -> RSolver () m a

stubSLD :: Monad m => SolverF m a -> Maybe Int -> [Rule ()] -> Term -> m a
stubSLD sldF limits kb g =
  let limitMap = M.fromList $ maybeToList $ ((), ) <$> limits
  in fst <$> runSLD (uncurry (sldF kb) (goal limitMap g))

stubSLDAll :: Maybe Int -> [Rule ()] -> Term -> [Unifier]
stubSLDAll limits kb = runIdentity . stubSLD sldAll limits kb

stubSLDSome :: Monad m => Maybe Int -> [Rule ()] -> Term -> m (Maybe Unifier)
stubSLDSome = stubSLD sldSome

stubGoal :: Limits String -> BoundedGoal String
stubGoal ls = Goal ls [] Store.empty

mkFailures :: [BoundedGoal String] -> FailureMap String
mkFailures = foldr putFailure M.empty

p :: Term -> Term
p = TFunctor "p" . (:[])

q :: Term -> Term
q = TFunctor "q" . (:[])

rules :: [Rule ()]
rules = Rule () (p x) [q x] : map (fact () . q) ints
  where
    x = TVar "x"

    ints :: [Term]
    ints = map TInt [0..2]

loopingRules :: [Rule ()]
loopingRules = [Rule () (p x) [p x]]
  where
    x = TVar "x"

lazilySolvable :: [Rule ()]
lazilySolvable =
    Rule () (p (TInt 1)) [q (TInt 1)]
  : fact () (q (TInt 1))
  : loopingRules

tests :: TestTree
tests =
  testGroup
    "Resolvable.Resolution"
    [ testGroup "SLD resolution"
      [ testCase "returns all matching alternatives" $
          let g = p $ TVar "x"
              us = stubSLDAll Nothing rules g
          in S.fromList (map (`apply` g) us) @?= S.fromList (map (p . TInt) [0..2])
      , testCase "sldAll is lazy" $ do
          let g = p $ TVar "x"
          result <- timeout 100000 $ do
            let us = stubSLDAll Nothing lazilySolvable g
            return $ (`apply` g) . fst <$> uncons us
          join result @?= Just (p (TInt 1))
      , testCase "sldAll is not lazy in IO" $ do
          let g = p $ TVar "x"
          result <- timeout 100000 $ do
            let (g0, u0) = goal M.empty g
            us <- fst <$> runSLD $ sldAll lazilySolvable g0 u0
            return $ (`apply` g) . fst <$> uncons us
          result @?= Nothing
      , testCase "sldSome is lazy in IO" $ do
          let g = p $ TVar "x"
          result <- timeout 100000 $ do
            u <- stubSLDSome Nothing lazilySolvable g
            return $ (`apply` g) <$> u
          join result @?= Just (p (TInt 1))
      , testCase "respects limits" $
          let g = p $ TVar "x"
          in null (stubSLDAll (Just 3) loopingRules g) @?= True
      , testCase "non-termination without limits" $
          let g = p $ TVar "x"
          in do
            result <- timeout 100000 $ evaluate (null (stubSLDAll Nothing loopingRules g))
            result @?= Nothing
      ]
    , testGroup "bounds"
      [ testCase "failure order (a)" $ limitsSmallerEq (M.fromList [("a", 1)]) (M.fromList [("a", 2)]) @?= True
      , testCase "failure order (a)" $ limitsSmallerEq (M.fromList [("a", 2)]) (M.fromList [("a", 2)]) @?= True
      , testCase "failure order (b)" $ limitsSmallerEq (M.fromList [("a", 1)]) (M.fromList [("a", 2), ("b", 3)]) @?= False
      , testCase "failure order (c)" $ limitsSmallerEq (M.fromList [("a", 2)]) (M.fromList [("a", 1)]) @?= False
      , testCase "putting failures maintains larger limits" $
        mkFailures [ stubGoal (M.fromList [("a", 1)]), stubGoal (M.fromList [("a", 2)]) ] @?= mkFailures [ stubGoal (M.fromList [("a", 2)]) ] ]
      , testCase "putting incomparable failures maintains both" $
        let incomp = [ M.fromList [("a", 1)], M.fromList [("a", 2), ("b", 3)] ]
            failures = mkFailures (map stubGoal incomp)
        in  maybe (-1) (length . fst) (M.maxView failures) @?= 2
      , testCase "fail on lower limits" $
        let failures = mkFailures [ stubGoal (M.fromList [("a", 2), ("b", 3)]) ]
        in  hasFailed (stubGoal $ M.fromList [("a", 2), ("b", 2)]) failures @?= True
      , testCase "don't fail on larger limits" $
        let failures = mkFailures [ stubGoal (M.fromList [("a", 2), ("b", 3)]) ]
        in  hasFailed (stubGoal $ M.fromList [("a", 4), ("b", 2)]) failures @?= False ]
