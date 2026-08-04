{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , fromZone
  , NameServersConfig
  , mergeConfig
  , compileRRSet
  , compileConfig
  , plResolvable ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

import NameConstraint
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.ZoneFile

import Util.Fresh (Fresh)
import Util.Monad

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

synthesisCases :: [ConstrainedName -> Fresh (Term, [Term])]
synthesisCases = [nameWithoutSynthesis', nameWithSynthesis]
  where
    nameWithoutSynthesis' :: ConstrainedName -> Fresh (Term, [Term])
    nameWithoutSynthesis' n = return (nameWithoutSynthesis n, [])

resolvableBase :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
resolvableBase rootServers ns s@(Named n rds@((RData _ _ ty) NE.:| _)) =
  foreachM synthesisCases $ \synth ->
  foreachM rootKeyCases   $ \(ver, adb) -> do
    (n', cs) <- synth n
    return $ S.singleton $ Rule rann (resolves (nameTerm ns) (typeTerm ty) n' (rrsetTerm rds) bot ver adb) cs
    where
      -- TODO: Refine so that only root keys are trusted
      rootKeyCases :: [(Term, Term)]
      rootKeyCases
        -- Set the ad bit of DNSKEY entries stored at root servers to 1
        | toType ty == T.DNSKEY && ns `elem` rootServers = [(bot, ad 0), (top, ad 1)]
        | otherwise = [(bot, ad 0)]

      rann :: Maybe RComp
      rann = Just (RComp ns s)

resolvableStep :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
resolvableStep (map nameTerm -> rootServers) ns s@(Named n rs) =
  foreachM (NE.toList rs) (\r -> foreachM rootServers (`go` rtype r))
  where
    rannot :: Maybe RComp
    rannot = Just $ RComp ns s

    nameServer :: Term
    nameServer = nameTerm ns

    go :: Term -> Type -> Fresh (S.Set (Rule (Maybe RComp)))
    go rootServer r = case r of
      NS s' ->
        let delegatedNs = nameTerm s'
            expectGlue = isSubDomain s' (toName n)
            glueS = if expectGlue then nameServer else rootServer
            glueRec = if expectGlue then bot else top
        in if nameServer == delegatedNs
          then return S.empty
          else mconcat <$> sequence
            [ foreachM [aType, aaaaType] $ \glueType -> do  -- Unauthenticated
                (nTail, t, d, dIp, rec) <- freshVars5 ("N", "T", "D", "D_IP", "R")
                -- Do not expect wildcard neq constraints on NS records
                let (captureListTail nTail -> n') = nameWithoutSynthesis n
                return (S.singleton $ Rule rannot (resolves nameServer t n' d top bot (ad 0))
                  [ resolves delegatedNs t n' d rec bot (ad 0)
                  , resolves glueS glueType delegatedNs dIp glueRec bot (ad 0) ])
            , do  -- NSEC for unsigned subzone
                (nTail, t, d, dNSEC, rec) <- freshVars5 ("N", "T", "D", "D_NSEC", "R")
                let n' = nameWithoutSynthesis n
                let n'' = captureListTail nTail n'
                return $ S.singleton $ Rule rannot (resolves nameServer t n'' d top top (ad 0))
                  [ t \= dsType
                  , resolves nameServer nsecType n' (list [dNSEC]) bot top (ad 1)
                  , nsecUnsigned n' dNSEC
                  , resolves nameServer t n'' d rec bot (ad 0) ]
            , do  -- NSEC3 for owner for unsigned subzone
                (nTail, t, d, nNSEC, dNSEC, rec) <- freshVars6 ("N", "T", "D", "N_NSEC", "D_NSEC", "R")
                let owner = nameWithoutSynthesis n
                let n'' = captureListTail nTail owner
                return $ S.singleton $ Rule rannot (resolves nameServer t n'' d top top (ad 0))
                  [ t \= dsType
                  , resolves nameServer nsec3Type nNSEC (list [dNSEC]) bot top (ad 1)
                  , nsec3Unsigned owner nNSEC dNSEC
                  , resolves nameServer t n'' d rec bot (ad 0) ]
            , do  -- NSEC3 closest encloser proof for unsigned subzone
              (nTail, t, d, nNSEC, nNSEC', dNSEC, dNSEC', rec) <- freshVars8 ("N", "T", "D", "N_NSEC", "N_NSEC", "D_NSEC", "D_NSEC", "R")
              let owner = nameWithoutSynthesis n
              let n'' = captureListTail nTail owner
              return $ S.singleton $ Rule rannot (resolves nameServer t n'' d top top (ad 0))
                [ t \= dsType
                , resolves nameServer nsec3Type nNSEC (list [dNSEC]) bot top (ad 1)
                , resolves nameServer nsec3Type nNSEC' (list [dNSEC']) bot top (ad 1)
                , closestEncloser owner nNSEC dNSEC nNSEC' dNSEC'
                , resolves nameServer t n'' d rec bot (ad 0) ]
            , do  -- Authenticated not-DS
                (nTail, t, d, rec) <- freshVars4 ("N", "T", "D", "R")
                let (captureListTail nTail -> n') = nameWithoutSynthesis n
                return (S.singleton $ Rule rannot (resolves nameServer t n' d top top (ad 1))
                  [ t \= dsType
                  , resolves delegatedNs t n' d rec top (ad 1) ])
            , do  -- Authenticated DS
                (l, nTail, d, rec) <- freshVars4 ("L", "N", "D", "R")
                let (injectLast l . captureListTail nTail -> n') = nameWithoutSynthesis n
                return (S.singleton $ Rule rannot (resolves nameServer dsType n' d top top (ad 1))
                  [ resolves delegatedNs dsType n' d rec top (ad 1) ])
            ]
      CNAME (nameTerm -> cname) -> foreachM synthesisCases $ \synth -> S.fromList <$> sequence
        [ do  -- Unauthenticated
          (n', cs) <- synth n
          (t, d, rec) <- freshVars3 ("T", "D", "R")
          return $ Rule rannot (resolves nameServer t n' d top bot (ad 0))
            (cs ++  [ t \= cnameType
                          , resolves rootServer t cname d rec bot (ad 0) ])
        , do  -- Authenticated
          (n', cs) <- synth n
          (t, d, rec1, rec2, a) <- freshVars5 ("T", "D", "R", "R", "A")
          return $ Rule rannot (resolves nameServer t n' d top top a)
            (cs ++  [ t \= cnameType
                          , resolves nameServer cnameType (nameWithoutSynthesis n) (list [TComplex r]) rec1 top (ad 1)
                          , resolves rootServer t cname d rec2 top a ]) ]
      DNAME (nameTerm -> dname) -> S.fromList <$> sequence
        [ do  -- Unauthenticated
          (l, nTail, t, d, rec) <- freshVars5 ("L", "N", "T", "D", "R")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = nameWithoutSynthesis n
          return $ Rule rannot (resolves nameServer t (rewrite n') d top bot (ad 0))
            [ resolves rootServer t (rewrite dname) d rec bot (ad 0) ]
        , do  -- Authenticated
          (l, nTail, t, d, rec1, rec2, a) <- freshVars7 ("L", "N", "T", "D", "R", "R", "A")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = nameWithoutSynthesis n
          return $ Rule rannot (resolves nameServer t (rewrite n') d top top a)
            [ resolves nameServer dnameType n' (list [TComplex r]) rec1 top (ad 1)
            , resolves rootServer t (rewrite dname) d rec2 top a ] ]
      -- TODO: Check that sigName is the name of the zone containing the record;
      -- does that mean there ex. a corresponding SOA record? Could be, but seems
      -- technically wrong as there could be intermediate SOA records. It could be
      -- that this will be implicitly addressed by the ad(1) resolution of the
      -- DNSKEY record.
      sig@(RRSIG (RRSIGData cov _ _ _ _ _ _ (nameTerm -> sigName) _)) ->
        case cov of
          T.DNSKEY -> do
            let n' = nameWithoutSynthesis n
            (d, d_DS, k, kRef, rec) <- freshVars5 ("D", "D_DS", "K", "K_REF", "R")
            return $ S.singleton $ Rule rannot
              (resolves nameServer dnskeyType n' d bot top (ad 1))
              [ resolves nameServer dnskeyType n' d bot bot (ad 0)
                    , resolves rootServer dsType n' d_DS rec top (ad 1)
                    , member k d
                    , member kRef d_DS
                    , digestValid n' k kRef
                    , rrsigValid n' d (TComplex sig) k ]
          _ -> foreachM synthesisCases $ \synth -> do
            (n', cs) <- synth n
            (d, d_KEY, k) <- freshVars3 ("D", "D_KEY", "K")
            return $ S.singleton $ Rule rannot
              (resolves nameServer (ttypeTerm cov) n' d bot top (ad 1))
              (cs ++  [ resolves nameServer (ttypeTerm cov) n' d bot bot (ad 0)
                            , resolves nameServer dnskeyType sigName d_KEY bot top (ad 1)
                            , member k d_KEY
                            , rrsigValid n' d (TComplex sig) k ])
      A {} -> return S.empty
      AAAA {} -> return S.empty
      TXT {} -> return S.empty
      MX {} -> return S.empty
      SOA {} -> return S.empty
      NSEC {} -> return S.empty
      NSEC3 {} -> return S.empty
      NSEC3PARAM {} -> return S.empty
      DS {} -> return S.empty
      DNSKEY {} -> return S.empty

resolvableR :: Fresh (Rule (Maybe RComp))
resolvableR = do
  (s, q, n, rec, v, a) <- freshVars6 ("S", "Q", "N", "R", "V", "A")
  wc <- wildcard
  return $ Rule Nothing (resolvable s q n v a) [ resolves s q n wc rec v a ]

allResolvable :: Rule (Maybe RComp)
allResolvable =
  Rule Nothing (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (resolvable rns q n v a) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"
    v = TVar "Ver"
    a = TVar "Auth"

    bagof :: Term -> Term -> Term -> Term
    bagof x y z = TFunctor "bagof" [ x, y, z ]

compileRRSet :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
compileRRSet rootServers ns rs = (<>) <$> resolvableBase rootServers ns rs <*> resolvableStep rootServers ns rs

fromZone :: [Name String] -> Name String -> Zone ConstrainedLabel -> Fresh (S.Set (Rule (Maybe RComp)))
fromZone rootServers ns z = mconcat <$> mapM (compileRRSet rootServers ns) (allRRSets z)

type NameServersConfig = [(Name String, Zone ConstrainedLabel)]

mergeConfig :: NameServersConfig -> NameServersConfig
mergeConfig = M.toList . foldr (uncurry (M.insertWith mergeZones)) M.empty

compileConfig :: [Name String] -> NameServersConfig -> Fresh (S.Set (Rule (Maybe RComp)))
compileConfig rootServers = (mconcat <$>) . mapM (uncurry (fromZone rootServers))

plResolvable :: S.Set (Rule (Maybe RComp)) -> Fresh (S.Set (Rule (Maybe RComp)))
plResolvable compiled = do
  res <- resolvableR
  return $ foldr S.insert compiled [allResolvable, res]
