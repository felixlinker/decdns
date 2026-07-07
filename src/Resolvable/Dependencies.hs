{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Dependencies
  ( ResolvableAt(..)
  , Resolves(..)
  , matches
  , RecordDependencies
  , RuleDependencies
  , ConstrainedQuery(..)
  , fromRules
  , computeResolvable ) where

import Control.Arrow (Arrow(first))
import Control.Monad (when, guard)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import Data.List (uncons, partition)
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
import Control.Monad.Trans.Except (ExceptT, throwE)
import Util.Monad (exceptMaybe, foreachM)
import DNS.SEC.Verification (verifySignature, verifyDigest)
import Data.Bifunctor (Bifunctor(bimap))

class FromRule t where
  fromRule :: Rule (Maybe RComp) -> Maybe t

newtype ResolvableAt = ResolvableAt (Name String, ConstrainedName)
  deriving (Eq, Ord)

instance FromRule ResolvableAt where
  fromRule = fmap fromAnnot . rannotation

fromAnnot :: RComp -> ResolvableAt
fromAnnot (RComp ns (name -> owner)) = ResolvableAt (ns, owner)

newtype Resolves = Resolves (Term, Term, Term, Term)
  deriving (Eq, Ord)

instance Show Resolves where
  show (Resolves trip) = show trip

-- TODO:
fromTerm :: Term -> Maybe Resolves
fromTerm (toView -> TVResolves ns t n _ _ _ auth) = Just $ Resolves (canon ns, canon t, canon n, auth)
fromTerm _ = Nothing

instance FromRule Resolves where
  fromRule = fromTerm . rhead

matches :: Resolves -> Name String -> Name String -> Bool
matches (Resolves (nsT, nT, _, _)) ns n = unifies (list [nsT, nT]) (list [nameTerm ns, nameTerm n])

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

fromRules :: (FromRule a, Ord a) => [Rule (Maybe RComp)] -> PO.PartialOrder a
fromRules rs = go rs PO.empty
  where
    go :: (FromRule a, Ord a) => [Rule (Maybe RComp)] -> PO.PartialOrder a -> PO.PartialOrder a
    go [] acc = acc
    go (r:rrec) acc =
      let els = mapMaybe (match r) rs
      in go rrec (foldr (uncurry PO.insert . first S.singleton) acc els)

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

instance Show ConstrainedQuery where
  show = show . toRule

extendAndMerge :: Term -> ConstrainedQuery -> CU.ConstrainedUnifier -> Maybe CU.ConstrainedUnifier
extendAndMerge t (CQuery t' neqs) cu = do
  cu' <- CU.extendUnifier t t' cu
  foldr ((=<<) . uncurry CU.addConstraint) (Just cu') (CU.toList neqs)

asName :: Monad m => Term -> ExceptT String m (Name String)
asName t = do
  ts <- exceptMaybe ("name term is not a list: " ++ show t) $ collectList t
  ls <- exceptMaybe ("label not ground: " ++ show t) $ mapM asAtom ts
  -- Add empty label was we do not represent it in Prolog
  return $ Name (NE.reverse $ "" NE.:| ls)
  where
    asAtom :: Term -> Maybe String
    asAtom (TAtom a) = Just a
    asAtom _ = Nothing

asType :: Monad m => Term -> ExceptT String m Type
asType (TComplex d) = return d
asType t = throwE ("data not ground: " ++ show t)

asTypes :: Monad m => Term -> ExceptT String m [Type]
asTypes t = do
  l <- exceptMaybe ("data not ground: " ++ show t) $ collectList t
  mapM asType l

asDNSKEYData :: Monad m => Type -> ExceptT String m DNSKEYData
asDNSKEYData (DNSKEY dat) = return dat
asDNSKEYData t = throwE $ "expected DNSKEY got: " ++ show t

asDSData :: Monad m => Type -> ExceptT String m DSData
asDSData (DS dat) = return dat
asDSData t = throwE $ "expected DS got: " ++ show t

asRRSigData :: Monad m => Type -> ExceptT String m (RRSIGData BS.ByteString)
asRRSigData (RRSIG dat) = return dat
asRRSigData t = throwE $ "expected RRSIG got: " ++ show t

solveBody :: Monad m => S.Set ConstrainedQuery -> Rule a -> ExceptT String m (S.Set ConstrainedQuery)
solveBody (S.toList -> rs) (Rule _ h b) = S.fromList . map (\u -> CQuery (CU.apply u h) (CU.constraints u)) <$> rec b [CU.identity]
  where
    rec :: Monad m => [Term] -> [CU.ConstrainedUnifier] -> ExceptT String m [CU.ConstrainedUnifier]
    rec [] choices = return choices
    rec (t:recArg) choices = case toView t of
      TVNeq t1 t2 -> rec recArg (mapMaybe (CU.addConstraint t1 t2) choices)
      TVResolves {} -> rec recArg =<< foreachM choices
        (\u -> return $ mapMaybe (\cq -> extendAndMerge t cq u) rs)
      TVMember x l -> rec recArg =<< foreachM choices (\u -> do
        let ul = CU.apply u l
        l' <- exceptMaybe ("variable list for non-deterministic choice: " ++ show ul) (collectList ul)
        return $ mapMaybe (\el -> CU.extendUnifier x el u) l')
      TVDigestValid n dnskeyT dsT -> foreachM choices $ \u -> do
        let app = CU.apply u
        n' <- asName (app n)
        dsDat <- asDSData =<< asType (app dsT)
        dnskeyDat <- asDNSKEYData =<< asType (app dnskeyT)
        checkReturn (verifyDigest n' dsDat dnskeyDat) u
      TVRRSIGValid n dat sig k -> foreachM choices $ \u -> do
        let app = CU.apply u
        n' <- asName (app n)
        rrset <- asTypes (app dat)
        sigDat <- asRRSigData =<< asType (app sig)
        DNSKEYData _ _ k' <- asDNSKEYData =<< asType (app k)
        checkReturn (verifySignature n' rrset sigDat k') u
      -- This drops unsupported predicates, implicitly assuming that they are true
      _ -> rec recArg choices
      where
        checkReturn :: Monad m => Bool -> a -> ExceptT String m [a]
        checkReturn p a = return $ guard p >> [a]

computeResolvable :: Monad m => [Rule (Maybe RComp)] -> ExceptT String m (S.Set ConstrainedQuery)
computeResolvable (filter (isResolves . rhead) -> rs) = do
  ord <- exceptMaybe "cyclic dependencies" $ PO.transitiveClosure $ fromRules rs
  rec ord rs S.empty
  where
    rec :: Monad m => RuleDependencies -> [Rule (Maybe RComp)] -> S.Set ConstrainedQuery -> ExceptT String m (S.Set ConstrainedQuery)
    rec _ [] acc = return acc
    rec ord toSolve acc =
      let (mins, recArg) = partition (maybe False (PO.minimal ord) . fromRule) toSolve
      in do
        when (null mins) (throwE "non-termination")
        acc' <- mconcat <$> mapM (solveBody acc) mins
        rec (foldr (maybe id PO.remove . fromRule) ord mins) recArg (acc' <> acc)
