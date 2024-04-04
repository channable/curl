{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BinaryLiterals #-}

-- | Bindings to curl/urlapi.h
module Network.Curl.Url (
  -- * URL validation
  isValidUrl,
  isValidUrlIO,
  isValidUrl',
  isValidUrlIO',
  -- * urlapi.h flags
  Flags,
  pattern CURLU_DEFAULT_PORT,
  pattern CURLU_NO_DEFAULT_PORT,
  pattern CURLU_DEFAULT_SCHEME,
  pattern CURLU_NON_SUPPORT_SCHEME,
  pattern CURLU_PATH_AS_IS,
  pattern CURLU_DISALLOW_USER,
  pattern CURLU_URLDECODE,
  pattern CURLU_URLENCODE,
  pattern CURLU_APPENDQUERY,
  pattern CURLU_GUESS_SCHEME,
  pattern CURLU_NO_AUTHORITY,
  pattern CURLU_ALLOW_SPACE,
  pattern CURLU_PUNYCODE,
  pattern CURLU_PUNI2IDN,
) where

import Foreign (IntPtr (..))
import Foreign.C (CInt (..), CUInt (..))
import Foreign.C.String (CString)
import GHC.IO (unsafePerformIO)
import Data.Text (Text)
import Data.Text.Foreign (withCString)
import Control.Exception (bracket)
import Data.Bits

-- | Defaults for validation.
defaultFlags :: Flags
defaultFlags = CURLU_DEFAULT_SCHEME

-- | Ask cURL whether it would consider the given string a valid URL, using the
-- default flags:
-- 
-- * `CURLU_DEFAULT_SCHEME`
isValidUrl :: Text -> Bool
isValidUrl = isValidUrl' defaultFlags

-- | Ask cURL whether it would consider the given string a valid URL, using the
-- flags given as the first argument.
isValidUrl' :: Flags -> Text -> Bool
isValidUrl' flags = unsafePerformIO . isValidUrlIO' flags

-- | Ask cURL whether it would consider the given string a valid URL, using the
-- default flags:
-- 
-- * `CURLU_DEFAULT_SCHEME`
isValidUrlIO :: Text -> IO Bool
isValidUrlIO = isValidUrlIO' defaultFlags

-- | Ask cURL whether it would consider the given string a valid URL, using the
-- flags given as the first argument.
isValidUrlIO' :: Flags -> Text -> IO Bool
isValidUrlIO' (Flags flags) s =
  bracket curl_url curl_url_cleanup $ \handle -> do
    code <- withCString s $ \str ->
      -- cURL parses the string when we set the whole URL, and returns
      -- a code if the URL is bad.
      curl_url_set
        handle -- the CURLU*
        0      -- CURLUPART_URL
        str    -- the string (which gets copied)
        flags  -- flags toggle specific behaviors
    -- The string is a valid URL if cURL has no complaints about it; that is,
    -- the `curl_url_set` we did earlier returned `CURLUE_OK` (== 0)
    return (code == CURLUE_OK)

newtype Flags = Flags CUInt

instance Semigroup Flags where
  (Flags l) <> (Flags r) = Flags (l .|.  r)

instance Monoid Flags where
  mempty = Flags 0

-- functions imported from `curl/urlapi.h`

foreign import ccall unsafe
  "curl_url" curl_url :: IO IntPtr

foreign import ccall unsafe
  "curl_url_cleanup" curl_url_cleanup :: IntPtr -> IO ()

foreign import ccall unsafe
  "curl_url_set" curl_url_set :: IntPtr -> CInt -> CString -> CUInt -> IO CInt

pattern CURLUE_OK :: CInt
pattern CURLUE_OK = 0

-- Reference: https://github.com/curl/curl/blob/cfc65fd1ee164113e4b342f2e57e36fdc07c87fd/include/curl/urlapi.h#L84-L101

pattern CURLU_DEFAULT_PORT,
        CURLU_NO_DEFAULT_PORT,
        CURLU_DEFAULT_SCHEME,
        CURLU_NON_SUPPORT_SCHEME,
        CURLU_PATH_AS_IS,
        CURLU_DISALLOW_USER,
        CURLU_URLDECODE,
        CURLU_URLENCODE,
        CURLU_APPENDQUERY,
        CURLU_GUESS_SCHEME,
        CURLU_NO_AUTHORITY,
        CURLU_ALLOW_SPACE,
        CURLU_PUNYCODE,
        CURLU_PUNI2IDN
        :: Flags

pattern CURLU_DEFAULT_PORT       = Flags 0b1              -- 1 << 0
pattern CURLU_NO_DEFAULT_PORT    = Flags 0b10             -- 1 << 1
pattern CURLU_DEFAULT_SCHEME     = Flags 0b100            -- 1 << 2
pattern CURLU_NON_SUPPORT_SCHEME = Flags 0b1000           -- 1 << 3
pattern CURLU_PATH_AS_IS         = Flags 0b10000          -- 1 << 4
pattern CURLU_DISALLOW_USER      = Flags 0b100000         -- 1 << 5
pattern CURLU_URLDECODE          = Flags 0b1000000        -- 1 << 6
pattern CURLU_URLENCODE          = Flags 0b10000000       -- 1 << 7
pattern CURLU_APPENDQUERY        = Flags 0b100000000      -- 1 << 8
pattern CURLU_GUESS_SCHEME       = Flags 0b1000000000     -- 1 << 9
pattern CURLU_NO_AUTHORITY       = Flags 0b10000000000    -- 1 << 10
pattern CURLU_ALLOW_SPACE        = Flags 0b100000000000   -- 1 << 11
pattern CURLU_PUNYCODE           = Flags 0b1000000000000  -- 1 << 12
pattern CURLU_PUNI2IDN           = Flags 0b10000000000000 -- 1 << 13
