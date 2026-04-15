{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , plResolvable ) where

import NameConstraint
import Resolvable.Prolog as Pl
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T

import Util.Fresh (Fresh, fresh)
import Control.Monad (void)

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: ConstrainedRecord }
  deriving (Eq, Ord)

resolvableBase :: Name String -> ConstrainedRecord -> Fresh (Rule RComp)
resolvableBase ns r@(Named n (RData _ _ ty)) = do
  (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
  let cannot = RComp ns r
  return (Rule cannot (resolves (nameTerm ns) (typeAtom ty) n' (dataAtom ty) (ad 0)) neqs)

captureListTail :: Term -> Term -> Term
captureListTail captureWith t = case toView t of
  v@(TVList _ _) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec (TVList a b) = tList a (rec (toView b))
    rec _ = captureWith

resolvableStep :: [Name String] -> Name String -> ConstrainedRecord -> Fresh [Rule RComp]
resolvableStep (map nameTerm -> rootServers) ns r@(Named n (rtype -> NS (nameTerm -> delegatedNs))) = sequence $ do
  rootServer <- rootServers
  glueType <- ttypeAtom <$> [T.A, T.AAAA]
  return $ do
    t <- TVar <$> fresh "T"
    -- Do not expect wildcard neq constraints on NS records
    (captureListTail t -> n', _) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res (ad 0)
                , rbody = [ resolves delegatedNs q n' res (ad 0)
                          , resolves rootServer glueType delegatedNs TWildcard (ad 0) ] }
resolvableStep (map nameTerm -> rootServers) ns r@(Named n (rtype -> CNAME (nameTerm -> cname))) = sequence $ do
  rootServer <- rootServers
  return $ do
    (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res (ad 0)
                , rbody = neqs ++ [ resolves rootServer q cname res (ad 0) ] }
resolvableStep (map nameTerm -> rootServers) ns r@(Named n (rtype -> DNAME (nameTerm -> dname))) = sequence $ do
  rootServer <- rootServers
  return $ do -- Enter Fresh monad
    -- Do not expect wildcard neq constraints on CNAME records
    t <- TVar <$> fresh "T"
    (captureListTail t -> n', _) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res (ad 0)
                , rbody = [ t \= TEmptyList
                          , resolves rootServer q (captureListTail t dname) res (ad 0) ] }
resolvableStep _ _ (Named _ (rtype -> _)) = return []

resolvableR :: Fresh (Rule ())
resolvableR = do
  s <- TVar <$> fresh "S"
  q <- TVar <$> fresh "Q"
  n <- TVar <$> fresh "N"
  return $ Rule () (resolvable s q n) [ resolves s q n TWildcard (ad 0) ]

allResolvable :: Rule ()
allResolvable =
  Rule () (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (resolvable rns q n) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TFunctor "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> [ConstrainedRecord] -> Fresh [Rule RComp]
fromZoneFile rootServers ns rs = concat <$> mapM recordToRules rs
  where
    recordToRules :: ConstrainedRecord -> Fresh [Rule RComp]
    recordToRules r = (:) <$> resolvableBase ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [ConstrainedRecord])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule RComp]
compileConfig rootServers = (concat <$>) . mapM (uncurry (fromZoneFile rootServers))

plResolvable :: [Rule RComp] -> Fresh [Rule ()]
plResolvable compiled = do
  res <- resolvableR
  return $ allResolvable : res : map void compiled
