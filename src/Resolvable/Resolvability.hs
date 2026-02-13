{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , resolves
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , presolvable
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

resolves :: Term -> Term -> Term -> Term -> Term
resolves ns t n r = TPred "resolves" [ ns, t, n, r ]

resolvableBase :: Name String -> ConstrainedRecord -> Fresh (Rule RComp)
resolvableBase ns r@(Record n (RData _ _ ty)) = do
  (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
  let cannot = RComp ns r
  return (Rule cannot (resolves (nameTerm ns) (typeAtom ty) n' (dataAtom ty)) neqs)

captureListTail :: Term -> Term -> Term
captureListTail t (TList pref Nothing) = TList pref (Just t)
captureListTail _ (TList _ (Just _)) = undefined
captureListTail _ _ = undefined

resolvableStep :: [Name String] -> Name String -> ConstrainedRecord -> Fresh [Rule RComp]
resolvableStep (map nameTerm -> rootServers) ns r@(Record n (rtype -> NS (nameTerm -> delegatedNs))) = sequence $ do
  rootServer <- rootServers
  glueType <- ttypeAtom <$> [T.A, T.AAAA]
  return $ do
    t <- TVar <$> fresh "T"
    -- Do not expect wildcard neq constraints on NS records
    (captureListTail t -> n', _) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res
                , rbody = [ resolves delegatedNs q n' res
                          , resolves rootServer glueType delegatedNs TWildcard ] }
resolvableStep (map nameTerm -> rootServers) ns r@(Record n (rtype -> CNAME (nameTerm -> cname))) = sequence $ do
  rootServer <- rootServers
  return $ do
    (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res
                , rbody = neqs ++ [ resolves rootServer q cname res ] }
resolvableStep (map nameTerm -> rootServers) ns r@(Record n (rtype -> DNAME (nameTerm -> dname))) = sequence $ do
  rootServer <- rootServers
  return $ do -- Enter Fresh monad
    -- Do not expect wildcard neq constraints on CNAME records
    t <- TVar <$> fresh "T"
    (captureListTail t -> n', _) <- nameWithSynthesis n
    q <- TVar <$> fresh "Q"
    res <- TVar <$> fresh "R"
    return Rule { rannotation = RComp ns r
                , rhead = resolves (nameTerm ns) q n' res
                , rbody = [ t \= TList [] Nothing
                          , resolves rootServer q (captureListTail t dname) res ] }
resolvableStep _ _ (Record _ (rtype -> _)) = return []

presolvable :: Term -> Term -> Term -> Term
presolvable s q n = TPred "resolvable" [ s, q, n ]

resolvable :: Fresh (Rule ())
resolvable = do
  s <- TVar <$> fresh "S"
  q <- TVar <$> fresh "Q"
  n <- TVar <$> fresh "N"
  return $ Rule () (presolvable s q n) [ resolves s q n TWildcard ]

allResolvable :: Rule ()
allResolvable =
  Rule () (TPred "all_resolvable" [ q, rns, ns ]) [ bagof n (presolvable rns q n) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TPred "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> [ConstrainedRecord] -> Fresh [Rule RComp]
fromZoneFile rootServers ns rs = concat <$> mapM recordToRules rs
  where
    recordToRules :: ConstrainedRecord -> Fresh [Rule RComp]
    recordToRules r = (:) <$> resolvableBase ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [ConstrainedRecord])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule RComp]
compileConfig rootServers = (concat <$>) . mapM (uncurry (fromZoneFile rootServers))

plResolvable :: [Name String] -> NameServersConfig -> Fresh [Rule ()]
plResolvable rootServers cfg = do
  res <- resolvable
  compiled <- map void <$> compileConfig rootServers cfg
  return $ allResolvable : res : compiled
