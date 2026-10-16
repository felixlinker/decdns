{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Dependencies
  ( computeRegressions
  , resolvableQuery ) where

import qualified Data.Set as S
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

import qualified DNS.RecordType as T
import Resolvable.Consequences
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Util.Fresh
import Resolvable.Resolvability (RComp)
import Resolvable.Update.Disunification (withouts)

computeRegressions :: S.Set (Rule RComp) -> S.Set (Rule RComp) -> ExceptT String Fresh (S.Set Fact)
computeRegressions oldRs newRs = do
  resOld <- lift $ immediateConsequences oldRs
  resNew <- lift $ immediateConsequences newRs
  resOldQs <- S.toList <$> foldM toResolvable S.empty resOld
  resNewQs <- S.toList <$> foldM toResolvable S.empty resNew
  return $ S.fromList $ concatMap (`withouts` resNewQs) resOldQs
  where
    toResolvable :: Monad m => S.Set Fact -> Fact -> ExceptT String m (S.Set Fact)
    toResolvable s r = return $ maybe s (`S.insert` s) (resolvableQuery r)

-- TODO: Restrict neqs to variables that occur in new head term
resolvableQuery :: Fact -> Maybe Fact
resolvableQuery (Fact t neqs) = (`Fact` neqs) <$> resolvableFromResolves t

resolvableFromResolves :: Term -> Maybe Term
resolvableFromResolves (toView -> TVResolves r@(Resolves { resolvesZone = z, resolvesQType = qt }))
  -- Do not report referral data as resolvable
  | z == referralZone = Nothing
  -- Do not report structural records as resolvable
  | qt `elem` map ttypeTerm [T.CNAME, T.DNAME, T.SOA, T.NS, T.DS, T.DNSKEY, T.NSEC, T.NSEC3, T.NSEC3PARAM, T.RRSIG] = Nothing
  | otherwise = Just $ resolvable
    (resolvesZone r)
    (resolvesQType r)
    (resolvesQName r)
    (resolvesVerify r)
    (resolvesAD r)
resolvableFromResolves _ = Nothing
