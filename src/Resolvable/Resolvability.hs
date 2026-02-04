{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , resolves
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , plResolvable ) where

import NameConstraint
import Resolvable.Prolog as Pl
import DNS.ZoneFile
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T

import Util.Fresh (Fresh, fresh, runFresh)
import Control.Monad (void)

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: Record ConstrainedName }
  deriving (Eq, Ord)

resolves :: Term -> Term -> Term -> Term -> Term
resolves ns t n r = TPred "resolves" [ ns, t, n, r ]

resolvableBase :: Name String -> Record ConstrainedName -> Fresh (Rule RComp)
resolvableBase ns r@(Record n (RData _ _ ty)) = do
  (n', neqs) <- nameWithSynthesis n
  let cannot = RComp ns r
  return (Rule cannot (resolves (nameTerm ns) (typeAtom ty) n' (dataAtom ty)) neqs)

captureListTail :: Term -> Term -> Term
captureListTail t (TList pref Nothing) = TList pref (Just t)
captureListTail _ (TList _ (Just _)) = undefined
captureListTail _ _ = undefined

resolvableStep :: [Name String] -> Name String -> Record ConstrainedName -> Fresh [Rule RComp]
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
    (n', neqs) <- nameWithSynthesis n
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

resolvable :: Rule ()
resolvable =
  Rule () (TPred "resolvable" [ q, rns, ns ]) [ bagof n (r Pl.^ resolves rns q n r) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"
    r = TVar "R"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TPred "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> Zone (NameConstraint String) -> Fresh [Rule RComp]
fromZoneFile rootServers ns (allRecords -> rs) = concat <$> mapM recordToRules rs
  where
    recordToRules :: Record ConstrainedName -> Fresh [Rule RComp]
    recordToRules r = (:) <$> resolvableBase ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [Zone (NameConstraint String)])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule RComp]
compileConfig rootServers = (concat <$>) . mapM (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> Fresh [Rule RComp]
    compileOne ns = (concat <$>) . mapM (fromZoneFile rootServers ns)

plResolvable :: [Name String] -> NameServersConfig -> [Rule ()]
plResolvable rootServers = (resolvable :) . map void . runFresh . compileConfig rootServers
