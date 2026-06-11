{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , plResolvable ) where

import qualified Data.List.NonEmpty as NE

import NameConstraint
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import qualified Resolvable.Prolog.Library as L
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.ZoneFile

import Util.Fresh (Fresh, fresh)
import Control.Monad (void, guard)

injectLast :: Term -> Term -> Term
injectLast newLast t = case toView t of
  v@(TVList _ _ ) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec TVEmptyList = tList newLast emptyList
    rec (TVList a b) = tList a (rec (toView b))
    -- x cannot be a list
    rec x = tList newLast (fromView x)

captureListTail :: Term -> Term -> Term
captureListTail captureWith t = case toView t of
  v@(TVList _ _) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec (TVList a b) = tList a (rec (toView b))
    rec _ = captureWith

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: ConstrainedRRSet }
  deriving (Eq, Ord)

resolvableBase :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh (Rule RComp)
resolvableBase rootServers ns s@(Named n rds@((RData _ _ ty) NE.:| _)) = do
    (n', cs) <- L.nameWithSynthesis n
    let annot = RComp ns s
        -- TODO: Refine so that only root keys are trusted
        -- Set the ad bit of DNSKEY entries stored at root servers to 1
        adb = fromEnum (toType ty == T.DNSKEY && ns `elem` rootServers)
    return (Rule annot
      (L.resolves (L.nameTerm ns) (L.typeTerm ty) n' (L.rrsetTerm rds) (L.ad adb))
      cs)

one :: Functor f => f a -> f [a]
one = fmap return

foreach :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
foreach f = fmap concat . mapM f

resolvableStep :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh [Rule RComp]
resolvableStep (map L.nameTerm -> rootServers) ns s@(Named n rs) =
  foreach (\r -> foreach (`go` rtype r) rootServers) (NE.toList rs)
  where
    rannot :: RComp
    rannot = RComp ns s

    nameServer :: Term
    nameServer = L.nameTerm ns

    go :: Term -> Type -> Fresh [Rule RComp]
    go rootServer r = case r of
      NS (L.nameTerm -> delegatedNs) -> concat <$> sequence
        [ do  -- Unauthenticated
            (nTail, t, d) <- freshVars3 ("N", "T", "D")
            -- Do not expect wildcard neq constraints on NS records
            let (captureListTail nTail -> n') = L.nameWithoutSynthesis n
            return $ map
              (\glueType ->
                Rule rannot (L.resolves nameServer t n' d (L.ad 0))
                  [ L.resolves delegatedNs t n' d (L.ad 0)
                  , L.resolves rootServer glueType delegatedNs TWildcard TWildcard ])
              [L.aType, L.aaaaType]
        , one $ do  -- Authenticated
            (nTail, d) <- freshVars2 ("N", "D")
            let (captureListTail nTail -> n') = L.nameWithoutSynthesis n
            return $ Rule rannot (L.resolves nameServer nTail n' d (L.ad 1))
              [ L.resolves delegatedNs nTail n' d (L.ad 1) ] ]
      CNAME (L.nameTerm -> cname) -> sequence
        [ do  -- Unauthenticated
          (n', cs) <- L.nameWithSynthesis n
          (t, d) <- freshVars2 ("T", "D")
          return $ Rule rannot (L.resolves nameServer t n' d (L.ad 0))
            (cs ++ [ L.resolves rootServer t cname d (L.ad 0) ])
        , do  -- Authenticated
          (n', cs) <- L.nameWithSynthesis n
          (t, d) <- freshVars2 ("T", "D")
          return $ Rule rannot (L.resolves nameServer t n' d (L.ad 1))
            (cs ++  [ L.resolves nameServer L.cnameType (L.nameWithoutSynthesis n) (list [L.dataTerm r]) (L.ad 1)
                          , L.resolves rootServer t cname d (L.ad 1) ]) ]
      DNAME (L.nameTerm -> dname) -> sequence
        [ do  -- Unauthenticated
          (l, nTail, t, d) <- freshVars4 ("L", "N", "T", "D")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = L.nameWithoutSynthesis n
          return $ Rule rannot (L.resolves nameServer t (rewrite n') d (L.ad 0))
            [ L.resolves rootServer t (rewrite dname) d (L.ad 0) ]
        , do  -- Authenticated
          (l, nTail, t, d) <- freshVars4 ("L", "N", "T", "D")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = L.nameWithoutSynthesis n
          return $ Rule rannot (L.resolves nameServer t (rewrite n') d (L.ad 1))
            [ L.resolves nameServer L.dnameType n' (list [L.dataTerm r]) (L.ad 1)
            , L.resolves rootServer t (rewrite dname) d (L.ad 1) ] ]
      -- TODO: Check that sigName is the name of the zone containing the record;
      -- does that mean there ex. a corresponding SOA record? Could be, but seems
      -- technically wrong as there could be intermediate SOA records. It could be
      -- that this will be implicitly addressed by the ad(1) resolution of the
      -- DNSKEY record.
      (RRSIG cov (L.algTerm -> alg) (TInt -> ls) (TInt -> ottl) _ _ (TInt -> tag) (L.nameTerm -> sigName) (L.b64Term -> sig)) ->
        one $ case cov of
          T.DNSKEY -> do
            let n' = L.nameWithoutSynthesis n
            (d, d_DS, k, f, t_DIG, k_DIG) <- freshVars6 ("D", "D_DS", "K", "F", "T_DIG", "K_DIG")
            dnskey <- L.dnskey f <$> freshVar "A" <*> return k
            return $ Rule rannot
              (L.resolves nameServer L.dnskeyType n' d (L.ad 1))
              [ L.resolves nameServer L.dnskeyType n' d (L.ad 0)
                    , L.resolves rootServer L.dsType n' d_DS (L.ad 1)
                    , member dnskey d
                    , L.keyTag dnskey tag
                    , member (L.ds tag alg t_DIG k_DIG) d_DS
                    , L.digestValid t_DIG n' f alg k k_DIG
                    , L.rrsigValid n' L.dnskeyType ottl d ls sig k ]
          _ -> do
            (n', cs) <- L.nameWithSynthesis n
            (d, d_KEY, k, f) <- freshVars4 ("D", "D_KEY", "K", "F")
            dnskey <- L.dnskey f <$> freshVar "A" <*> return k
            return $ Rule rannot
              (L.resolves nameServer L.dnskeyType n' d (L.ad 1))
              (cs ++  [ L.resolves nameServer (L.ttypeTerm cov) n' d (L.ad 0)
                            , L.resolves rootServer L.dnskeyType sigName d_KEY (L.ad 1)
                            , member dnskey d
                            , L.keyTag dnskey tag
                            , L.rrsigValid n' (L.ttypeTerm cov) ottl d ls sig k ])
      A {} -> return []
      AAAA {} -> return []
      TXT {} -> return []
      MX {} -> return []
      SOA {} -> return []
      NSEC {} -> return []
      DS {} -> return []
      DNSKEY {} -> return []

resolvableR :: Fresh (Rule ())
resolvableR = do
  s <- freshVar "S"
  q <- freshVar "Q"
  n <- freshVar "N"
  return $ Rule () (L.resolvable s q n) [ L.resolves s q n TWildcard (L.ad 0) ]

allResolvable :: Rule ()
allResolvable =
  Rule () (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (L.resolvable rns q n) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TFunctor "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> [Zone ConstrainedLabel] -> Fresh [Rule RComp]
fromZoneFile rootServers ns rs = concat <$> mapM zoneToRules rs
  where
    zoneToRules :: Zone ConstrainedLabel -> Fresh [Rule RComp]
    zoneToRules z = concat <$> mapM rrsetToRules (allRRSets z)

    rrsetToRules :: ConstrainedRRSet -> Fresh [Rule RComp]
    rrsetToRules r = (:) <$> resolvableBase rootServers ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [Zone ConstrainedLabel])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule RComp]
compileConfig rootServers = (concat <$>) . mapM (uncurry (fromZoneFile rootServers))

plResolvable :: [Rule RComp] -> Fresh [Rule ()]
plResolvable compiled = do
  res <- resolvableR
  return $ allResolvable : res : map void compiled
