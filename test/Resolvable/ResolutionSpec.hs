{-# LANGUAGE TupleSections #-}
module Resolvable.ResolutionSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (maybeToList)
import Control.Exception (evaluate)
import System.Timeout (timeout)

import Resolvable.Prolog.Core
import Resolvable.Resolution (goal, runSLD, sld)
import Resolvable.Unification (Unifier, apply)

stubSLD :: Maybe Int -> [Rule ()] -> Term -> [Unifier]
stubSLD limits kb g =
  let limitMap = M.fromList $ maybeToList $ ((), ) <$> limits
  in fst $ runIdentity $ runSLD limitMap $ uncurry (sld kb) (goal g)

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

tests :: TestTree
tests =
  testGroup
    "Resolvable.Resolution"
    [ testCase "returns all matching alternatives" $
        let g = p $ TVar "x"
            us = stubSLD Nothing rules g
        in S.fromList (map (`apply` g) us) @?= S.fromList (map (p . TInt) [0..2])
    , testCase "respects limits" $
        let g = p $ TVar "x"
        in null (stubSLD (Just 3) loopingRules g) @?= True
    , testCase "non-termination without limits" $
        let g = p $ TVar "x"
        in do
          result <- timeout 100000 $ evaluate (null (stubSLD Nothing loopingRules g))
          result @?= Nothing
    ]
