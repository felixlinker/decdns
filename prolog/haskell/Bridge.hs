{-# LANGUAGE ForeignFunctionInterface #-}
module Bridge (dnsCheck) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.Except (runExcept)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as UTF8
import Data.Char (toUpper)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, WordPtr(..), nullPtr)
import Foreign.Storable (peek, poke, pokeByteOff)
import Text.Read (readMaybe)
import Text.Parsec (eof, runParser)

import DNS.Record (Type(..))
import DNS.SEC.Verification
import Parsing.Data (recordTypeData)
import Parsing.Bytes (hex)
import Parsing.State (emptyState)
import Resolvable.Prolog.Core (Term(..), collectList)
import Resolvable.Prolog.Reverse (asName, asDSData, asDNSKEYData, asRRSigData, asNSECData, asNSEC3Data)
import Util.Bytes (toB32)

foreign import ccall unsafe "dns_term_kind" termKind :: WordPtr -> IO CInt
foreign import ccall unsafe "dns_term_text" termText :: WordPtr -> IO CString
foreign import ccall unsafe "dns_text_free" freeText :: CString -> IO ()
foreign import ccall unsafe "dns_term_functor" termFunctor :: WordPtr -> Ptr CSize -> IO CString
foreign import ccall unsafe "dns_term_arg" termArg :: WordPtr -> CSize -> IO WordPtr
foreign import ccall unsafe "dns_list_part" listPart :: WordPtr -> CInt -> IO WordPtr
foreign export ccall "dns_check" dnsCheck :: Int32 -> WordPtr -> Ptr CString -> IO Int32

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
      args <- mapM (\i -> termArg term i >>= copyTerm (depth + 1)) [1..arity]
      pure (TFunctor name args)
    _ -> fail "expected an atom, string, integer, list, or record compound"

copyText :: CString -> IO String
copyText ptr
  | ptr == nullPtr = fail "invalid text (embedded NUL is unsupported)"
  | otherwise = do
      bytes <- BS.packCString ptr
      freeText ptr
      either (fail . show) (pure . Text.unpack) (UTF8.decodeUtf8' bytes)

-- Reuse the project's RDATA parser rather than duplicate its constructors,
-- algorithm decoding, and binary decoders. Names are root-first Prolog lists.
recordData :: Term -> Either String Type
recordData (TFunctor "txt" [TAtom text]) = Right (TXT text)
recordData term = do
  text <- presentation term
  either (Left . show) Right (runParser (recordTypeData <* eof) emptyState "Prolog RDATA" text)

presentation :: Term -> Either String String
presentation (TFunctor name fields) = case (name, fields) of
  ("dnskey", [flags, alg, key]) -> do
    fs <- number 65535 flags
    a <- number 255 alg
    k <- string key
    pure (unwords ["DNSKEY", fs, "3", a, k])
  ("ds", [tag, alg, digest, bytes]) -> do
    t <- number 65535 tag
    a <- number 255 alg
    d <- number 255 digest
    unless (d `elem` ["1", "2", "4"]) (Left "unsupported DS digest algorithm")
    b <- string bytes
    pure (unwords ["DS", t, a, d, b])
  ("rrsig", [ty, alg, labels, ttl, expiration, inception, tag, signer, sig]) -> do
    t <- typeName ty
    a <- number 255 alg
    l <- number 255 labels
    times <- mapM (number 4294967295) [ttl, expiration, inception]
    k <- number 65535 tag
    n <- nameText signer
    s <- string sig
    pure (unwords (["RRSIG", t, a, l] ++ times ++ [k, n, s]))
  ("nsec", [next, types]) -> do
    n <- nameText next
    ts <- properList types >>= mapM typeName
    pure (unwords ("NSEC" : n : ts))
  ("nsec3", [alg, flags, iterations, salt, next, types]) -> do
    a <- number 255 alg
    f <- number 1 flags
    i <- number 65535 iterations
    s <- saltText salt
    b <- string next >>= decodeHex
    unless (BS.length b == 20) (Left "NSEC3 next hash must contain 20 bytes")
    ts <- properList types >>= mapM typeName
    pure (unwords (["NSEC3", a, f, i, s, toB32 b] ++ ts))
  ("nsec3param", [alg, flags, iterations, salt]) -> do
    a <- number 255 alg
    f <- number 1 flags
    i <- number 65535 iterations
    s <- saltText salt
    pure (unwords ["NSEC3PARAM", a, f, i, s])
  ("a", [ip]) -> ("A " ++) <$> atom ip
  ("aaaa", [ip]) -> ("AAAA " ++) <$> atom ip
  ("txt", [text]) -> do
    s <- atom text
    unless (all (`notElem` ['"', '\\', '\n', '\r']) s) (Left "TXT contains unsupported presentation characters")
    pure ("TXT \"" ++ s ++ "\"")
  (ty, [nameArg]) | ty `elem` ["ns", "cname", "dname"] ->
    ((map toUpper ty ++ " ") ++) <$> nameText nameArg
  ("mx", [pref, exchange]) -> do
    p <- number 65535 pref
    n <- nameText exchange
    pure (unwords ["MX", p, n])
  ("soa", [mname, rname, serial, refresh, retry, expiry, minimumTTL]) -> do
    ns <- mapM nameText [mname, rname]
    values <- mapM (number 4294967295) [serial, refresh, retry, expiry, minimumTTL]
    pure (unwords ("SOA" : ns ++ values))
  _ -> Left ("unsupported record constructor: " ++ name)
presentation _ = Left "expected a record compound"

number :: Integer -> Term -> Either String String
number upper (TInt n)
  | n >= 0 && toInteger n <= upper = Right (show n)
number _ _ = Left "invalid numeric record field"

atom :: Term -> Either String String
atom (TAtom s) = Right s
atom _ = Left "expected an atom"

string :: Term -> Either String String
string (TString s) = Right s
string _ = Left "expected a binary field encoded as a string"

typeName :: Term -> Either String String
typeName t = map toUpper <$> atom t

properList :: Term -> Either String [Term]
properList = maybe (Left "expected a proper list") Right . collectList

nameText :: Term -> Either String String
nameText t = show <$> runExcept (asName t)

decodeHex :: String -> Either String BS.ByteString
decodeHex "" = Right BS.empty
decodeHex text = either (Left . show) Right (runParser (hex <* eof) () "hex field" text)

saltText :: Term -> Either String String
saltText t = do
  s <- string t
  bytes <- decodeHex s
  when (BS.length bytes > 255) (Left "NSEC3 salt exceeds 255 bytes")
  pure (if null s then "-" else s)

verify :: Int32 -> Term -> Either String Bool
verify op arguments = do
  args <- properList arguments
  case (op, args) of
    (0, [name, key, ds]) -> do
      n <- runExcept (asName name)
      k <- recordData key >>= runExcept . asDNSKEYData
      d <- recordData ds >>= runExcept . asDSData
      pure (verifyDigest n d k)
    (1, [name, rrset, sig, key]) -> do
      n <- runExcept (asName name)
      rs <- properList rrset >>= mapM recordData
      s <- recordData sig >>= runExcept . asRRSigData
      k <- recordData key >>= runExcept . asDNSKEYData
      pure (verifySignature n rs s k)
    (2, [name, nsec]) -> do
      n <- runExcept (asName name)
      dat <- recordData nsec >>= runExcept . asNSECData
      pure (nsecUnsignedSubzone n n dat)
    (3, [name, hashedOwner, nsec3]) -> do
      n <- runExcept (asName name)
      h <- runExcept (asName hashedOwner)
      dat <- recordData nsec3 >>= runExcept . asNSEC3Data
      pure (nsec3UnsignedSubzone n h dat)
    (4, [name, existing, existingData, missing, missingData]) -> do
      n <- runExcept (asName name)
      e <- runExcept (asName existing)
      ed <- recordData existingData >>= runExcept . asNSEC3Data
      m <- runExcept (asName missing)
      md <- recordData missingData >>= runExcept . asNSEC3Data
      pure (verifyClosestEncloserProof n e ed m md)
    _ -> Left "unknown operation or wrong argument count"

dnsCheck :: Int32 -> WordPtr -> Ptr CString -> IO Int32
dnsCheck op args errorPtr = do
  result <- try (copyTerm 0 args >>= either fail evaluate . verify op) :: IO (Either SomeException Bool)
  case result of
    Right valid -> pure (if valid then 1 else 0)
    Left err -> do
      let bytes = UTF8.encodeUtf8 (Text.pack (displayException err))
      ptr <- mallocBytes (BS.length bytes + 1)
      BS.useAsCString bytes $ \source -> copyBytes ptr source (BS.length bytes)
      pokeByteOff ptr (BS.length bytes) (0 :: Word8)
      poke errorPtr ptr
      pure (-1)
