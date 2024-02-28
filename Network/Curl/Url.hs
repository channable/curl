{-# LANGUAGE PatternSynonyms #-}

-- | Bindings to curl/urlapi.h
module Network.Curl.Url (isValidUrl, isValidUrlIO) where

import Foreign (IntPtr (..))
import Foreign.C (CInt (..), CUInt (..))
import Foreign.C.String (CString)
import GHC.IO (unsafePerformIO)
import Data.Text (Text)
import Data.Text.Foreign (withCString)
import Control.Exception (bracket)

-- | Ask cURL whether it would consider the given string a valid URL.
--
-- cURL attempts to adhere to RFC 3986.
isValidUrl :: Text -> Bool
isValidUrl = unsafePerformIO . isValidUrlIO

isValidUrlIO :: Text -> IO Bool
isValidUrlIO s =
  bracket curl_url curl_url_cleanup $ \handle -> do
    code <- withCString s $ \str ->
      -- cURL parses the string when we set the whole URL, and returns
      -- a code if the URL is bad.
      curl_url_set
        handle -- the CURLU*
        0      -- CURLUPART_URL
        str    -- the string (which gets copied)
        0      -- dont pass any CURLU flags
    -- The string is a valid URL if cURL has no complaints about it; that is,
    -- the `curl_url_set` we did earlier returned `CURLUE_OK` (== 0)
    return (code == CURLUE_OK)

-- functions imported from `curl/urlapi.h`

foreign import ccall unsafe
  "curl_url" curl_url :: IO IntPtr

foreign import ccall unsafe
  "curl_url_cleanup" curl_url_cleanup :: IntPtr -> IO ()

foreign import ccall unsafe
  "curl_url_set" curl_url_set :: IntPtr -> CInt -> CString -> CUInt -> IO CInt

pattern CURLUE_OK :: CInt
pattern CURLUE_OK = 0
