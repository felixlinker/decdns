{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Resolvable.Constraint.Store
  ( ConstraintStore(..)
  , awaySt
  , partitionConstraints
  , empty
  , toBody
  , add
  , Resolvable.Constraint.Store.null
  , simplify
  , SimplifyCache
  , simplifyCached
  , evalCached
  , Resolvable.Constraint.Store.solve ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Bitraversable (bitraverse)
import qualified Control.Monad.Trans.Except as Ex
import qualified Control.Monad.Trans.State as St
import Data.List (intercalate, partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes, fromMaybe)

import DNS.SEC.Verification
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse
import qualified Resolvable.Unification as U
import qualified Resolvable.Constraint.Neqs as N
import Resolvable.Constraint.Solving
import Util.List

simplifyTEx :: Term -> Ex.Except String Bool
simplifyTEx t = case toView t of
  TVForAll vs (toView -> TVNeq a b)
    | not (wellformedQuant vs a b) -> Ex.throwE "unsupported quantifier"
    | ground a -> return $ not $ U.unifies a b
    -- TODO: Could also check that variable assignments are to ground terms *for
    -- variables occurring on the right-hand side**
    | otherwise -> maybe (return True) (\u -> guard (U.isRenaming u) >> return False) (U.unify a b)
  TVNot c -> not <$> simplifyTEx c
  TVIsName n
    | minNameLength n > 255 -> return False
    | ground n -> return True
  TVDigestValid n dnskeyT dsT | ground t -> do
      n' <- asName n
      dsDat <- asDSData =<< asType dsT
      dnskeyDat <- asDNSKEYData =<< asType dnskeyT
      return $ verifyDigest n' dsDat dnskeyDat
  TVRRSIGValid n dat sig k | ground t -> do
      n' <- asName n
      rrset <- asTypes dat
      sigDat <- asRRSigData =<< asType sig
      k' <- asDNSKEYData =<< asType k
      return (verifySignature n' rrset sigDat k')
  TVNSECUnsigned n nsecDat | ground t -> do
      n' <- asName n
      nsecDat' <- asNSECData =<< asType nsecDat
      return $ nsecUnsignedSubzone n' n' nsecDat'
  TVNSEC3Unsigned n hashedOwner nsec3Dat | ground t -> do
      n' <- asName n
      hashedOwner' <- asName hashedOwner
      nsec3Dat' <- asNSEC3Data =<< asType nsec3Dat
      return $ nsec3UnsignedSubzone n' hashedOwner' nsec3Dat'
  TVClosestEncloser n exN exDat nonExN nonExDat | ground t -> do
      n' <- asName n
      exN' <- asName exN
      exDat' <- asNSEC3Data =<< asType exDat
      nonExN' <- asName nonExN
      nonExDat' <- asNSEC3Data =<< asType nonExDat
      return $ verifyClosestEncloserProof n' exN' exDat' nonExN' nonExDat'
  _ -> Ex.throwE "cannot solve"

resultToSolved :: Bool -> Conjunction (Maybe Term)
resultToSolved b = if b then Conjunction Nothing else Bot

simplifyT :: Term -> Conjunction (Maybe Term)
simplifyT t =
  either  (const $ Conjunction $ Just t)
          resultToSolved
          (Ex.runExcept $ simplifyTEx t)

simplifyTCached :: Monad m => Term -> SimplifyCache m (Conjunction (Maybe Term))
simplifyTCached t = do
  mB <- St.gets (M.lookup t)
  maybe setCache (return . resultToSolved) mB
  where
    setCache :: Monad m => SimplifyCache m (Conjunction (Maybe Term))
    setCache = do
      either  (const $ return $ Conjunction $ Just t)
              (\b -> St.modify (M.insert t b) >> return (resultToSolved b))
              (Ex.runExcept $ simplifyTEx t)

data ConstraintStore = CStore N.Neqs [Term]
  deriving (Eq, Ord)

instance Show ConstraintStore where
  show cs = intercalate ", " $ map show (toBody cs)

instance U.Apply ConstraintStore where
  apply u (CStore ns os) = CStore (U.apply u ns) (U.apply u os)

instance Semigroup ConstraintStore where
  CStore ns os <> CStore ns' os' = CStore (ns <> ns') (os <> os')

awaySt :: Monad m => ConstraintStore -> U.AwayT m ConstraintStore
awaySt (CStore neqs ts) = do
  neqs' <- traverse (bitraverse U.awayT U.awayT) neqs
  ts' <- traverse U.awayT ts
  return $ CStore neqs' ts'

partitionConstraints :: [Term] -> (ConstraintStore, [Term])
partitionConstraints = foldr addConstraint (empty, [])
  where
    addConstraint :: Term -> (ConstraintStore, [Term]) -> (ConstraintStore, [Term])
    addConstraint t (st, b) = maybe (st, t:b) (, b) (add st t)

empty :: ConstraintStore
empty = CStore N.empty []

toBody :: ConstraintStore -> [Term]
toBody (CStore ns os) = N.asPredicates ns ++ os

wellformedQuant :: [String] -> Term -> Term -> Bool
wellformedQuant (S.fromList . map TVar -> vs) left right =
  freeVars right == vs && S.disjoint (freeVars left) vs

simplifyUniversalNeqs :: Term -> Disjunction [Term]
simplifyUniversalNeqs t = case t of
  TForAll [] qt
    -> Disjunction [qt]
  TForAll vs (toView -> TVNeq tl tr) | wellformedQuant vs tl tr
    -> do
      simplified <- simplifyDisj $ N.simplifyNeq tl tr
      let grouped = groupDisj [] $ catMaybes simplified
      concat <$> mapM (simplifyFurther . quantifyDisj) grouped
  _ -> Disjunction [t]
  where
    simplifyFurther :: Term -> Disjunction [Term]
    simplifyFurther t'
      | t' == t = Disjunction [t']
      | otherwise = simplifyUniversalNeqs t'

    varsOverlap :: Term -> [Term] -> Bool
    varsOverlap _ [] = True
    varsOverlap a bs = not $ all (S.disjoint (vars a) . vars) bs

    groupDisj :: [N.Neqs] -> N.Neqs -> [N.Neqs]
    groupDisj acc [] = acc
    groupDisj acc (n:rec) =
      let (overlap, acc') = partition (varsOverlap (snd n) . map snd) acc
      in  groupDisj ((n : concat overlap) : acc') rec

    quantifyDisj :: N.Neqs -> Term
    quantifyDisj [] = undefined
    quantifyDisj [(tl, tr)] = TForAll (snub $ varNames tr) (tl \= tr)
    quantifyDisj neqs@(a:as) =
      let quant = snub $ concatMap (varNames . snd) neqs
      in TForAll quant (assoc lor $ NE.map (uncurry (\=)) $ a NE.:| as)

addNeq :: ConstraintStore -> Term -> Maybe ConstraintStore
addNeq (CStore ns os) t = case toView t of
  TVNeq a b -> Just $ CStore (N.addConstraint a b ns) os
  _ -> Nothing

add :: ConstraintStore -> Term -> Maybe ConstraintStore
add st@(CStore ns os) t = addNeq st t <|> addNonNeq
  where
    addNonNeq :: Maybe ConstraintStore
    addNonNeq = if isConstraint t
      then Just $ CStore ns (t:os)
      else Nothing

isConstraint :: Term -> Bool
isConstraint t = case toView t of
  TVNeq {} -> True
  TVForAll {} -> True
  TVIsName {} -> True
  TVDigestValid {} -> True
  TVRRSIGValid {} -> True
  TVNSECUnsigned {} -> True
  TVNSEC3Unsigned {} -> True
  TVClosestEncloser {} -> True
  TVNot constraint -> isConstraint constraint
  _ -> False

null :: ConstraintStore -> Bool
null (CStore ns o) = Prelude.null ns && Prelude.null o

doubleNeg :: Term -> Term
doubleNeg t = case toView t of
  TVNot (toView -> TVNot c) -> doubleNeg c
  _ -> t

syntacticNots :: [Term] -> Conjunction [Term]
syntacticNots [] = Conjunction []
syntacticNots ((doubleNeg -> h):r) = case toView h of
  TVNot c | c `elem` r  -> Bot
  _ | nott h `elem` r   -> Bot
    | otherwise         -> Conjunction (h:) <*> syntacticNots r

-- Returns a disjunction
simplify :: ConstraintStore -> [ConstraintStore]
simplify = runIdentity . simplifyWithCache (Identity . simplifyT)

simplifyCached :: Monad m => ConstraintStore -> SimplifyCache m [ConstraintStore]
simplifyCached = simplifyWithCache simplifyTCached

type SimplifyCache m a = St.StateT (M.Map Term Bool) m a

simplifyWithCache :: forall m. Monad m => (Term -> m (Conjunction (Maybe Term))) -> ConstraintStore -> m [ConstraintStore]
simplifyWithCache simp (CStore ns ts) = disjunction (return [empty]) (fmap concat . mapM logical) rewrite
  where
    logical :: ConstraintStore -> m [ConstraintStore]
    logical (CStore ns' ts') = do
      let nsSimp = N.simplify ns'
      tsSimp <- fmap catMaybes . sequenceA <$> mapM simp ts'
      return $ conjunction [] (\ts'' -> map (`CStore` ts'') nsSimp) tsSimp

    -- Use syntactic transformations first; returns a disjunction
    rewrite :: Disjunction [ConstraintStore]
    rewrite = do
      ts' <- conjunction (Disjunction []) return $ syntacticNots ts
      concat <$> mapM toStore (cnfToDnf (map simplifyUniversalNeqs ts'))
      where
        -- Keep ns because above we only simplified ts
        toStore :: Conjunction [Term] -> Disjunction [ConstraintStore]
        toStore Bot = Disjunction []
        toStore (Conjunction ts') = Disjunction [foldr (\c st' -> fromMaybe (error "invariant error") (addNeq st' c <|> add st' c)) (CStore ns []) ts']

evalCached :: Monad m => SimplifyCache m a -> m a
evalCached = (`St.evalStateT` M.empty)

solve :: [ConstraintStore] -> Maybe Bool
solve [] = Just False
solve stores
  | any Resolvable.Constraint.Store.null stores = Just True
  | otherwise = Nothing
