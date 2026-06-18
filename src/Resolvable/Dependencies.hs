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
import Control.Monad (when, (<=<))
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
import Control.Monad.Trans.Except (Except, throwE)
import Util.Monad (exceptMaybe)
import DNS.SEC.Verification (verifySignature, verifyDigest)

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

fromTerm :: Term -> Maybe Resolves
fromTerm (toView -> TVResolves ns t n _ auth) = Just $ Resolves (canon ns, canon t, canon n, auth)
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

toRule :: ConstrainedQuery -> Rule ()
toRule (CQuery q (S.toList -> neqs)) = Rule () q (map (uncurry (\=)) neqs)

instance Show ConstrainedQuery where
  show = show . toRule

extendAndMerge :: Term -> ConstrainedQuery -> CU.ConstrainedUnifier -> Maybe CU.ConstrainedUnifier
extendAndMerge t (CQuery t' neqs) cu = do
  cu' <- CU.extendUnifier t t' cu
  foldr ((=<<) . uncurry CU.addConstraint) (Just cu') neqs

asName :: Term -> Except String (Name String)
asName t = do
  ts <- exceptMaybe "name term is not a list" $ collectList t
  ls <- exceptMaybe "label not ground" $ mapM asString ts
  -- Add empty label was we do not represent it in Prolog
  return $ Name (NE.reverse $ "" NE.:| ls)
  where
    asString :: Term -> Maybe String
    asString (TString s) = Just s
    asString _ = Nothing

asType :: Term -> Except String Type
asType (TComplex d) = return d
asType _ = throwE "data not ground"

asTypes :: Term -> Except String [Type]
asTypes = mapM asType <=< exceptMaybe "data not ground" . collectList

asDNSKEYData :: Type -> Except String DNSKEYData
asDNSKEYData (DNSKEY dat) = return dat
asDNSKEYData _ = throwE "data of wrong type"

asDSData :: Type -> Except String DSData
asDSData (DS dat) = return dat
asDSData _ = throwE "data of wrong type"

asRRSigData :: Type -> Except String (RRSIGData BS.ByteString)
asRRSigData (RRSIG dat) = return dat
asRRSigData _ = throwE "data of wrong type"

solveBody :: [ConstrainedQuery] -> Rule a -> Except String [ConstrainedQuery]
solveBody rs (Rule _ h b) = map (\u -> CQuery (CU.apply u h) (CU.constraints u)) <$> rec [CU.identity] b
  where
    rec :: [CU.ConstrainedUnifier] -> [Term] -> Except String [CU.ConstrainedUnifier]
    rec choices [] = return choices
    rec choices (t:recArg) = case toView t of
      TVNeq t1 t2 -> rec (mapMaybe (CU.addConstraint t1 t2) choices) recArg
      TVResolves {} -> rec (extendChoicesWith (extendAndMerge t) rs) recArg
      TVMember x l -> do
        l' <- exceptMaybe "variable list for non-deterministic choice" (collectList l)
        return $ extendChoicesWith (CU.extendUnifier x) l'
      TVDigestValid n dsT dnskeyT -> do
        n' <- asName n
        dsDat <- asDSData =<< asType dsT
        dnskeyDat <- asDNSKEYData =<< asType dnskeyT
        check $ verifyDigest n' dsDat dnskeyDat
      TVRRSIGValid n dat sig k -> do
        n' <- asName n
        rrset <- asTypes dat
        sigDat <- asRRSigData =<< asType sig
        DNSKEYData _ _ k' <- asDNSKEYData =<< asType k
        check $ verifySignature n' rrset sigDat k'
      -- This drops unsupported predicates, implicitly assuming that they are true
      _ -> rec choices recArg
      where
        check :: Bool -> Except String [CU.ConstrainedUnifier]
        check p = if p then return choices else return []

        extendChoicesWith :: (a -> CU.ConstrainedUnifier -> Maybe CU.ConstrainedUnifier) -> [a] -> [CU.ConstrainedUnifier]
        extendChoicesWith f as = concatMap (\u -> mapMaybe (`f` u) as) choices

-- TODO: Use Except monad transformer
computeResolvable :: [Rule (Maybe RComp)] -> Except String [ConstrainedQuery]
computeResolvable (filter (isResolves . rhead) -> rs) = do
  ord <- exceptMaybe "cyclic dependencies" $ PO.transitiveClosure $ fromRules rs
  rec ord rs []
  where
    rec :: RuleDependencies -> [Rule (Maybe RComp)] -> [ConstrainedQuery] -> Except String [ConstrainedQuery]
    rec _ [] acc = return acc
    rec ord toSolve acc =
      let (mins, recArg) = partition (maybe False (PO.minimal ord) . fromRule) toSolve
      in do
        when (null mins) (throwE "non-termination")
        acc' <- concat <$> mapM (solveBody acc) mins
        rec (foldr (maybe id PO.remove . fromRule) ord mins) recArg (acc' ++ acc)
