{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Dependencies
  ( ResolvableAt(..)
  , Resolves(..)
  , matches
  , RecordDependencies
  , RuleDependencies
  , ConstrainedQuery(..)
  , queryFromRule
  , fromRules
  , computeResolvable ) where

import Control.Monad (when, filterM, guard)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)

import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification
import DNS.Name
import DNS.Record
import qualified Resolvable.ConstrainedUnification as CU
import qualified Util.PartialOrder as PO
import Resolvable.Resolvability (RComp (RComp))
import NameConstraint (ConstrainedName)
import Control.Monad.Trans.Except (ExceptT, Except, runExcept, throwE)
import Util.Monad (exceptMaybe)
import DNS.SEC.Verification (verifySignature, verifyDigest, nsecUnsignedSubzone, nsec3UnsignedSubzone, verifyClosestEncloserProof)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Control.Monad.Trans.State as St

class FromRule t where
  fromRule :: Rule (Maybe RComp) -> Maybe t

newtype ResolvableAt = ResolvableAt (Name String, ConstrainedName)
  deriving (Eq, Ord)

instance FromRule ResolvableAt where
  fromRule = fmap fromAnnot . rannotation

-- TODO: I should be able to rely on the rule contents here too.
fromAnnot :: RComp -> ResolvableAt
fromAnnot (RComp ns (name -> owner)) = ResolvableAt (ns, owner)

newtype Resolves = Resolves (Term, Term, Term, Term, Term, Term)
  deriving (Eq, Ord)

instance Show Resolves where
  show (Resolves trip) = show trip

fromTerm :: Term -> Maybe Resolves
fromTerm (toView -> TVResolves ns t n _ recurse verify auth) = Just $ Resolves (canon ns, canon t, canon n, canon recurse, canon verify, auth)
fromTerm _ = Nothing

instance FromRule Resolves where
  fromRule = fromTerm . rhead

matches :: Resolves -> Name String -> Name String -> Bool
matches (Resolves (nsT, nT, _, _, _, _)) ns n = unifies (list [nsT, nT]) (list [nameTerm ns, nameTerm n])

canon :: Term -> Term
canon = replace aux
  where
    aux :: Term -> Term
    aux (TVar _) = TWildcard
    aux t = t

type RecordDependencies = PO.PartialOrder ResolvableAt

type RuleDependencies = PO.PartialOrder Resolves

instance Show RuleDependencies where
  show po = unlines (map (\(l, r) -> show l ++ " < " ++ show r) (PO.toFlatList po))

match :: FromRule a => Rule (Maybe RComp) -> Rule (Maybe RComp) -> Maybe (a, a)
match rl@(Rule _ (toView -> TVResolves {}) b) rs@(Rule _ h'@(toView -> TVResolves {}) _) = do
  (u, _) <- uncons $ mapMaybe (`unify` h') b
  -- Check whether there are any inequalities that prevent unification immediately
  _ <- CU.applyRule (CU.fromUnifier u) rs
  larger <- fromRule rl
  smaller <- fromRule rs
  return (smaller, larger)
match _ _ = Nothing

fromRules :: (FromRule a, Ord a) => S.Set (Rule (Maybe RComp)) -> PO.PartialOrder a
fromRules rs = foldr go PO.empty rs
  where
    go :: (FromRule a, Ord a) => Rule (Maybe RComp) -> PO.PartialOrder a -> PO.PartialOrder a
    go r acc =
      foldr (\r' foldAcc -> maybe foldAcc (\(S.singleton -> sml, lrg) -> PO.insert sml lrg foldAcc) (match r r')) acc rs

-- TODO: Refactor
data ConstrainedQuery = CQuery Term CU.Neqs
  deriving (Eq, Ord)

instance Apply (Maybe ConstrainedQuery) where
  apply _ Nothing = Nothing
  apply u (Just (CQuery t neqs)) = do
    let app = apply u
    neqs' <- CU.fromList (map (bimap app app) (CU.toList neqs))
    return $ CQuery (apply u t) neqs'

toRule :: ConstrainedQuery -> Rule ()
toRule (CQuery q neqs) = Rule () q (CU.asPredicates neqs)

queryFromRule :: Rule a -> Maybe ConstrainedQuery
queryFromRule (Rule _ h b) = do
  (rest, cu) <- grabNeqs b CU.identity
  guard (null rest)
  return $ CQuery h (CU.constraints cu)

grabNeqs :: [Term] -> CU.ConstrainedUnifier -> Maybe ([Term], CU.ConstrainedUnifier)
grabNeqs ts = St.runStateT (filterM importNeq ts)
  where
    importNeq :: Term -> St.StateT CU.ConstrainedUnifier Maybe Bool
    importNeq bodyT = case toView bodyT of
      TVNeq x y -> do
        St.modifyM (CU.addConstraint x y)
        return False
      _ -> return True

instance Show ConstrainedQuery where
  show = show . toRule

matchHead :: Term -> Rule a -> CU.ConstrainedUnifier -> Maybe (CU.ConstrainedUnifier, [Term])
matchHead t (Rule _ h b) cu = do
  cu' <- CU.extendUnifier t h cu
  (b', cu'') <- grabNeqs b cu'
  return (cu'', CU.apply cu'' b')

asName :: Term -> Except String (Name String)
asName t = do
  ts <- exceptMaybe ("name term is not a list: " ++ show t) $ collectList t
  ls <- exceptMaybe ("label not ground: " ++ show t) $ mapM asAtom ts
  -- Add empty label was we do not represent it in Prolog
  return $ Name (NE.reverse $ "" NE.:| ls)
  where
    asAtom :: Term -> Maybe String
    asAtom (TAtom a) = Just a
    asAtom _ = Nothing

asType :: Term -> Except String Type
asType (TComplex d) = return d
asType t = throwE ("data not ground: " ++ show t)

asTypes :: Term -> Except String [Type]
asTypes t = do
  l <- exceptMaybe ("data not ground: " ++ show t) $ collectList t
  mapM asType l

asDNSKEYData :: Type -> Except String DNSKEYData
asDNSKEYData (DNSKEY dat) = return dat
asDNSKEYData t = throwE $ "expected DNSKEY got: " ++ show t

asDSData :: Type -> Except String DSData
asDSData (DS dat) = return dat
asDSData t = throwE $ "expected DS got: " ++ show t

asRRSigData :: Type -> Except String (RRSIGData BS.ByteString)
asRRSigData (RRSIG dat) = return dat
asRRSigData t = throwE $ "expected RRSIG got: " ++ show t

asNSECData :: Type -> Except String NSECData
asNSECData (NSEC dat) = return dat
asNSECData t = throwE $ "expected NSEC got: " ++ show t

asNSEC3Data :: Type -> Except String NSEC3Data
asNSEC3Data (NSEC3 dat) = return dat
asNSEC3Data t = throwE $ "expected NSEC3 got: " ++ show t

solveBody :: Ord a => S.Set (Rule a) -> Rule a -> S.Set (Rule a)
solveBody rs (Rule ann h b) = S.fromList $ map (\(u, b') -> CU.apply u $ Rule ann h b') $ foldr solvePredicate [(CU.identity, [])] b
  where
    solvePredicate :: Term -> [(CU.ConstrainedUnifier, [Term])] -> [(CU.ConstrainedUnifier, [Term])]
    solvePredicate t choices = case toView t of
      TVNeq t1 t2 -> mapMaybe (\(u, accB) -> (,accB) <$> CU.addConstraint t1 t2 u) choices
      TVResolves {} -> concatMap
        (\(u, accB) -> foldr
          (\cq acc -> maybe acc (:acc) $ do
            (u', b') <- matchHead t cq u
            return (u', CU.apply u' accB ++ b'))
          []
          rs)
        choices
      TVMember x l -> concatMap
        (\(u, accB) -> fromRight [(u, accB)] $ runExcept $ do
          let ul = CU.apply u l
          l' <- exceptMaybe "member" (collectList ul)
          return $ mapMaybe (\el -> do
            u' <- CU.extendUnifier x el u
            return (u', CU.apply u' accB))
            l')
        choices
      TVDigestValid n dnskeyT dsT -> filter
        (\(u, _) -> fromRight True $ runExcept $ do
          let app = CU.apply u
          n' <- asName (app n)
          dsDat <- asDSData =<< asType (app dsT)
          dnskeyDat <- asDNSKEYData =<< asType (app dnskeyT)
          return (verifyDigest n' dsDat dnskeyDat))
        choices
      TVRRSIGValid n dat sig k -> filter
        (\(u, _) -> fromRight True $ runExcept $ do
          let app = CU.apply u
          n' <- asName (app n)
          rrset <- asTypes (app dat)
          sigDat <- asRRSigData =<< asType (app sig)
          DNSKEYData _ _ k' <- asDNSKEYData =<< asType (app k)
          return (verifySignature n' rrset sigDat k'))
        choices
      TVNSECUnsigned n nsecDat -> filter
        (\(u, _) -> fromRight True $ runExcept $ do
          let app = CU.apply u
          n' <- asName (app n)
          nsecDat' <- asNSECData =<< asType (app nsecDat)
          return $ nsecUnsignedSubzone n' n' nsecDat')
        choices
      TVNSEC3Unsigned n hashedOwner nsec3Dat -> filter
        (\(u, _) -> fromRight True $ runExcept $ do
          let app = CU.apply u
          n' <- asName (app n)
          hashedOwner' <- asName (app hashedOwner)
          nsec3Dat' <- asNSEC3Data =<< asType (app nsec3Dat)
          return $ nsec3UnsignedSubzone n' hashedOwner' nsec3Dat')
        choices
      TVClosestEncloser n exN exDat nonExN nonExDat -> filter
        (\(u, _) -> fromRight True $ runExcept $ do
          let app = CU.apply u
          n' <- asName (app n)
          exN' <- asName (app exN)
          exDat' <- asNSEC3Data =<< asType (app exDat)
          nonExN' <- asName (app nonExN)
          nonExDat' <- asNSEC3Data =<< asType (app nonExDat)
          return $ verifyClosestEncloserProof n' exN' exDat' nonExN' nonExDat')
        choices
      -- This drops unsupported predicates, implicitly assuming that they are true
      _ -> choices

computeResolvable :: Monad m => S.Set (Rule (Maybe RComp)) -> ExceptT String m (S.Set (Rule (Maybe RComp)))
computeResolvable (S.filter (isResolves . rhead) -> rs) = do
  ord <- exceptMaybe "cyclic dependencies" $ PO.transitiveClosure $ fromRules rs
  rec ord rs S.empty
  where
    rec :: Monad m => RuleDependencies -> S.Set (Rule (Maybe RComp)) -> S.Set (Rule (Maybe RComp)) -> ExceptT String m (S.Set (Rule (Maybe RComp)))
    rec ord toSolve acc
      | S.null toSolve = return acc
      | otherwise =
        let (mins, recArg) = S.partition (maybe False (PO.minimal ord) . fromRule) toSolve
        in do
          when (S.null mins) (throwE "non-termination")
          let acc' = foldr (\r -> (solveBody acc r <>)) acc mins
          rec (foldr (maybe id PO.remove . fromRule) ord mins) recArg (acc' <> mins)
