{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}
module Prolog.Bridge (dnsCheck) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (when, (>=>))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as UTF8
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, WordPtr(..), nullPtr)
import Foreign.Storable (peek, poke, pokeByteOff)
import Text.Read (readMaybe)

import DNS.SEC.Verification
import Resolvable.Prolog.Core (Term(..))
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse
import Util.Except

foreign import ccall unsafe "dns_term_kind" termKind :: WordPtr -> IO CInt
foreign import ccall unsafe "dns_term_text" termText :: WordPtr -> IO CString
foreign import ccall unsafe "dns_text_free" freeText :: CString -> IO ()
foreign import ccall unsafe "dns_term_functor" termFunctor :: WordPtr -> Ptr CSize -> IO CString
foreign import ccall unsafe "dns_term_arg" termArg :: WordPtr -> CSize -> IO WordPtr
foreign import ccall unsafe "dns_list_part" listPart :: WordPtr -> CInt -> IO WordPtr
foreign export ccall "dns_check" dnsCheck :: WordPtr -> Ptr CString -> IO Int32

-- The C adapter passes native SWI term references, not a textual serialization.
-- Copy the complete ground tree before invoking the pure verifier.
copyTerm :: Int -> WordPtr -> IO Term
copyTerm depth term = do
  when (depth > 1024 || term == 0) (fail "term too deep or invalid term reference")
  kind <- termKind term
  case kind of
    0 -> pure TEmptyList
    1 -> do
      h <- listPart term 0 >>= copyTerm (depth + 1)
      t <- listPart term 1 >>= copyTerm (depth + 1)
      pure (TFunctor "." [h, t])
    2 -> TAtom <$> (termText term >>= copyText)
    3 -> TString <$> (termText term >>= copyText)
    4 -> do
      text <- termText term >>= copyText
      maybe (fail "integer outside Haskell Int range") (pure . TInt) (readMaybe text)
    5 -> alloca $ \arityPtr -> do
      name <- termFunctor term arityPtr >>= copyText
      arity <- peek arityPtr
      when (arity > 16) (fail "unsupported record arity")
      args <- mapM (termArg term >=> copyTerm (depth + 1)) [1..arity]
      pure (TFunctor name args)
    _ -> fail "expected an atom, string, integer, list, or record compound"

copyText :: CString -> IO String
copyText ptr
  | ptr == nullPtr = fail "invalid text (embedded NUL is unsupported)"
  | otherwise = do
      bytes <- BS.packCString ptr
      freeText ptr
      either (fail . show) (pure . Text.unpack) (UTF8.decodeUtf8' bytes)

verify :: Term -> ExceptM Bool
verify (toView -> p) = do
  case p of
    TVRRSIGValid nam dat sig key -> do
      n <- toExceptM $ asName nam
      rs <- mapM asRRSetData =<< toExceptM (asList dat)
      s <- asRRSetData sig >>= toExceptM . asRRSigData
      k <- asRRSetData key >>= toExceptM . asDNSKEYData
      pure (verifySignature n rs s k)
    TVDigestValid nam ds key -> do
      n <- toExceptM (asName nam)
      k <- asRRSetData key >>= toExceptM . asDNSKEYData
      d <- asRRSetData ds >>= toExceptM . asDSData
      pure (verifyDigest n d k)
    TVNSECUnsigned nam nsec -> do
      n <- toExceptM (asName nam)
      dat <- asRRSetData nsec >>= toExceptM . asNSECData
      pure (nsecUnsignedSubzone n n dat)
    TVClosestEncloser nam exNam exDat nonExNam nonExDat -> do
      n <- toExceptM (asName nam)
      e <- toExceptM (asName exNam)
      ed <- asRRSetData exDat >>= toExceptM . asNSEC3Data
      m <- toExceptM (asName nonExNam)
      md <- asRRSetData nonExDat >>= toExceptM . asNSEC3Data
      pure (verifyClosestEncloserProof n e ed m md)
    TVNSEC3Unsigned nam hashedNam nsecDat -> do
      n <- toExceptM (asName nam)
      h <- toExceptM (asName hashedNam)
      dat <- asRRSetData nsecDat >>= toExceptM . asNSEC3Data
      pure (nsec3UnsignedSubzone n h dat)
    _ -> fail "unknown predicate"

dnsCheck :: WordPtr -> Ptr CString -> IO Int32
dnsCheck args errorPtr = do
  result <- try (copyTerm 0 args >>= either fail evaluate . runExceptM . verify) :: IO (Either SomeException Bool)
  case result of
    Right valid -> pure (if valid then 1 else 0)
    Left err -> do
      let bytes = UTF8.encodeUtf8 (Text.pack (displayException err))
      ptr <- mallocBytes (BS.length bytes + 1)
      BS.useAsCString bytes $ \source -> copyBytes ptr source (BS.length bytes)
      pokeByteOff ptr (BS.length bytes) (0 :: Word8)
      poke errorPtr ptr
      pure (-1)
