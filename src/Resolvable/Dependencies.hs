module Resolvable.Dependencies
  ( Dependencies(..)
  , fromRules ) where

import qualified Data.Map.Strict as M
import Data.List (partition, uncons)

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Unification
import ZoneFile
import NameConstraint
import Control.Monad (guard)

type RecordInstance = (Name String, Record ConstrainedName)

toInstance :: Rule RComp -> RecordInstance
toInstance (Rule (RComp ns r _) _ _) = (ns, r)

newtype Dependencies = Dependencies (M.Map RecordInstance [RecordInstance])

instance Show Dependencies where
  show (Dependencies m) = unlines $ map (uncurry showDep) (M.toList m)
    where
      showDep :: RecordInstance -> [RecordInstance] -> String
      showDep (ns, r) rs = "record " ++ show r ++ " at " ++ show ns ++ " resolves on: " ++ show (map snd rs)

add :: RecordInstance -> RecordInstance -> Dependencies -> Dependencies
add from to (Dependencies m) = Dependencies (M.insertWith (++) from [to] m)

fromRules :: [Rule RComp] -> Dependencies
fromRules allRs = go (Dependencies M.empty) allRs
  where
    go :: Dependencies -> [Rule RComp] -> Dependencies
    go ds [] = ds
    go ds ((Rule (RComp ns r rVar) _ b):rs) =
      let (withR, withoutR) = partition (subterm rVar) b
          bPart = do
            (rTerm, rTerm') <- uncons withR
            guard (null rTerm')
            return (rTerm, withoutR)
      in go (maybe ds (uncurry $ forRule ds allRs) bPart) rs
      where
        forRule :: Dependencies -> [Rule RComp] -> Term -> [Term] -> Dependencies
        forRule ds'' [] _ _ = ds''
        forRule ds'' (r':rs') rTerm tDeps =
          let match = matchHead rTerm r'
              matchesParallel = concatMap ((`filter` allRs) . matchHead) tDeps
              dsResult = if match then add (ns, r) (toInstance r') ds'' else ds''
              dsTDep = if match then foldr (add (toInstance r') . toInstance) dsResult matchesParallel else dsResult
          in forRule dsTDep rs' rTerm tDeps
