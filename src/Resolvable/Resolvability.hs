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
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.ZoneFile

import Util.Fresh (Fresh)
import Util.Monad
import qualified DNS.SEC.Keys as Keys

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

resolvableBase :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh [Rule (Maybe RComp)]
resolvableBase rootServers ns s@(Named n rds@((RData _ _ ty) NE.:| _)) = do
    (n', cs) <- nameWithSynthesis n
    -- TODO: Refine so that only root keys are trusted
    -- Set the ad bit of DNSKEY entries stored at root servers to 1
    let isDNSKEY = toType ty == T.DNSKEY
    -- TODO: At least also emit for 0
    let adb = ad $ fromEnum (isDNSKEY && ns `elem` rootServers)
    let keytagFacts = if isDNSKEY
        then map (keyTagFact . rtype) (NE.toList rds)
        else []
    return  ( Rule rann
        (resolves (nameTerm ns) (typeTerm ty) n' (rrsetTerm rds) adb)
        cs
            : keytagFacts)
    where
      rann :: Maybe RComp
      rann = Just (RComp ns s)

      keyTagFact :: Type -> Rule (Maybe RComp)
      keyTagFact d@(DNSKEY a b c) = fact rann (keyTag (TComplex d) (TInt $ fromEnum (Keys.keyTag a b c)))
      keyTagFact _ = undefined

resolvableStep :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh [Rule (Maybe RComp)]
resolvableStep (map nameTerm -> rootServers) ns s@(Named n rs) =
  foreachM (NE.toList rs) (\r -> foreachM rootServers (`go` rtype r))
  where
    rannot :: Maybe RComp
    rannot = Just $ RComp ns s

    nameServer :: Term
    nameServer = nameTerm ns

    go :: Term -> Type -> Fresh [Rule (Maybe RComp)]
    go rootServer r = case r of
      NS (nameTerm -> delegatedNs) -> concat <$> sequence
        [ do  -- Unauthenticated
            (nTail, t, d) <- freshVars3 ("N", "T", "D")
            -- Do not expect wildcard neq constraints on NS records
            let (captureListTail nTail -> n') = nameWithoutSynthesis n
            return $ map
              (\glueType ->
                Rule rannot (resolves nameServer t n' d (ad 0))
                  [ resolves delegatedNs t n' d (ad 0)
                  , resolves rootServer glueType delegatedNs TWildcard (ad 0) ])
              [aType, aaaaType]
        , one $ do  -- Authenticated not-DS
            (nTail, t, d) <- freshVars3 ("N", "T", "D")
            let (captureListTail nTail -> n') = nameWithoutSynthesis n
            return $ Rule rannot (resolves nameServer t n' d (ad 1))
              [ t \= dsType
              , resolves delegatedNs t n' d (ad 1) ]
        , one $ do  -- Authenticated DS
            (l, nTail, d) <- freshVars3 ("L", "N", "D")
            let (injectLast l . captureListTail nTail -> n') = nameWithoutSynthesis n
            return $ Rule rannot (resolves nameServer dsType n' d (ad 1))
              [ resolves delegatedNs dsType n' d (ad 1) ] ]
      CNAME (nameTerm -> cname) -> sequence
        [ do  -- Unauthenticated
          (n', cs) <- nameWithSynthesis n
          (t, d) <- freshVars2 ("T", "D")
          return $ Rule rannot (resolves nameServer t n' d (ad 0))
            (cs ++ [ resolves rootServer t cname d (ad 0) ])
        , do  -- Authenticated
          (n', cs) <- nameWithSynthesis n
          (t, d) <- freshVars2 ("T", "D")
          return $ Rule rannot (resolves nameServer t n' d (ad 1))
            (cs ++  [ t \= cnameType
                          , resolves nameServer cnameType (nameWithoutSynthesis n) (list [dataTerm r]) (ad 1)
                          , resolves rootServer t cname d (ad 1) ]) ]
      DNAME (nameTerm -> dname) -> sequence
        [ do  -- Unauthenticated
          (l, nTail, t, d) <- freshVars4 ("L", "N", "T", "D")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = nameWithoutSynthesis n
          return $ Rule rannot (resolves nameServer t (rewrite n') d (ad 0))
            [ resolves rootServer t (rewrite dname) d (ad 0) ]
        , do  -- Authenticated
          (l, nTail, t, d) <- freshVars4 ("L", "N", "T", "D")
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          let n' = nameWithoutSynthesis n
          return $ Rule rannot (resolves nameServer t (rewrite n') d (ad 1))
            [ resolves nameServer dnameType n' (list [dataTerm r]) (ad 1)
            , resolves rootServer t (rewrite dname) d (ad 1) ] ]
      -- TODO: Check that sigName is the name of the zone containing the record;
      -- does that mean there ex. a corresponding SOA record? Could be, but seems
      -- technically wrong as there could be intermediate SOA records. It could be
      -- that this will be implicitly addressed by the ad(1) resolution of the
      -- DNSKEY record.
      sig@(RRSIG (RRSIGData cov (algTerm -> alg) _ _ _ _ (TInt -> tag) (nameTerm -> sigName) _)) ->
        one $ case cov of
          T.DNSKEY -> do
            let n' = nameWithoutSynthesis n
            (d, d_DS, k, f, t_DIG, k_DIG) <- freshVars6 ("D", "D_DS", "K", "F", "T_DIG", "K_DIG")
            dnskeyTerm <- dnskey f <$> freshVar "A" <*> return k
            return $ Rule rannot
              (resolves nameServer dnskeyType n' d (ad 1))
              [ resolves nameServer dnskeyType n' d (ad 0)
                    , resolves rootServer dsType n' d_DS (ad 1)
                    , member dnskeyTerm d
                    , member (ds tag alg t_DIG k_DIG) d_DS
                    , digestValid t_DIG n' f alg k k_DIG
                    , rrsigValid n' d (TComplex sig) k ]
          _ -> do
            (n', cs) <- nameWithSynthesis n
            (d, d_KEY, k, f) <- freshVars4 ("D", "D_KEY", "K", "F")
            dnskeyTerm <- dnskey f <$> freshVar "A" <*> return k
            return $ Rule rannot
              (resolves nameServer (ttypeTerm cov) n' d (ad 1))
              (cs ++  [ resolves nameServer (ttypeTerm cov) n' d (ad 0)
                            , resolves nameServer dnskeyType sigName d_KEY (ad 1)
                            , member dnskeyTerm d
                            , keyTag dnskeyTerm tag
                            , rrsigValid n' d (TComplex sig) k ])
      A {} -> return []
      AAAA {} -> return []
      TXT {} -> return []
      MX {} -> return []
      SOA {} -> return []
      NSEC {} -> return []
      DS {} -> return []
      DNSKEY {} -> return []

resolvableR :: Fresh (Rule (Maybe RComp))
resolvableR = do
  s <- freshVar "S"
  q <- freshVar "Q"
  n <- freshVar "N"
  return $ Rule Nothing (resolvable s q n) [ resolves s q n TWildcard (ad 0) ]

allResolvable :: Rule (Maybe RComp)
allResolvable =
  Rule Nothing (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (resolvable rns q n) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TFunctor "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> [Zone ConstrainedLabel] -> Fresh [Rule (Maybe RComp)]
fromZoneFile rootServers ns rs = concat <$> mapM zoneToRules rs
  where
    zoneToRules :: Zone ConstrainedLabel -> Fresh [Rule (Maybe RComp)]
    zoneToRules z = concat <$> mapM rrsetToRules (allRRSets z)

    rrsetToRules :: ConstrainedRRSet -> Fresh [Rule (Maybe RComp)]
    rrsetToRules r = (++) <$> resolvableBase rootServers ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [Zone ConstrainedLabel])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule (Maybe RComp)]
compileConfig rootServers = (concat <$>) . mapM (uncurry (fromZoneFile rootServers))

plResolvable :: [Rule (Maybe RComp)] -> Fresh [Rule (Maybe RComp)]
plResolvable compiled = do
  res <- resolvableR
  return $ allResolvable : res : compiled
