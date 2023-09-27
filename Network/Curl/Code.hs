{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Code
-- Copyright : (c) Galois Inc 2007-2009, 2011
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Curl's status codes as a Haskell type.
--
--------------------------------------------------------------------

module Network.Curl.Code where

import Foreign.C.Types

-- Pattern synonyms to map the very old error codes to their new names. Old
-- constructors are kept for backwards compatibility.
pattern CurlNotBuiltIn = CurlUrlMalformatUser
pattern CurlWeirdServerReply = CurlFtpWeirdServerReply
pattern CurlRemoteAccessDenied = CurlFtpAccessDenied
pattern CurlFtpAcceptFailed = CurlFtpUserPasswordIncorrect
pattern CurlFtpAcceptTimeout = CurlFtpWeirdUserReply
pattern CurlHttp2 = CurlFtpCantReconnect
pattern CurlFtpCouldnSetType = CurlFtpCouldnSetBinary
pattern CurlObsoleteError20 = CurlFtpWriteError
pattern CurlQuoteError = CurlFtpQuoteError
pattern CurlObsoleteError24 = CurlMalformatError
pattern CurlUploadFailed = CurlFtpCouldnStorFile
pattern CurlObsoleteError29 = CurlFtpCouldntSetAscii
pattern CurlObsoleteError32 = CurlFtpCouldntGetSize
pattern CurlRangeError = CurlHttpRangeError
pattern CurlObsoleteError40 = CurlLibraryNotFound
pattern CurlObsoleteError44 = CurlBadCallingOrder
pattern CurlObsoleteError46 = CurlBadPasswordEntered
pattern CurlUnknownOption = CurlUnknownTelnetOption
pattern CurlSetOptOptionSyntax = CurlTelnetOptionSyntax
pattern CurlObsoleteError50 = CurlObsolete
pattern CurlObsoleteError51 = CurlSSLPeerCertificate
pattern CurlObsoleteError57 = CurlShareInUse
pattern CurlSslCipher = CurlSSLCipher
pattern CurlPeerFailedVerification = CurlSSLCACert
pattern CurlObsoleteError62 = CurlLDAPInvalidUrl
pattern CurlUseSslFailed = CurlFtpSSLFailed
pattern CurlSslEngineInitFailed = CurlSSLEngineInitFailed
pattern CurlRemoteDiskFull = CurlTFtpDiskFull
pattern CurlRemoteFileExists = CurlTFtpExists
pattern CurlObsoleteError75 = CurlConvFailed
pattern CurlObsoleteError76 = CurlConvReqd

data CurlCode
 = CurlOK
 | CurlUnspportedProtocol
 | CurlFailedInit
 | CurlUrlMalformat
 | CurlUrlMalformatUser
 | CurlCouldntResolveProxy
 | CurlCouldntResolveHost
 | CurlCouldntConnect
 | CurlFtpWeirdServerReply
 | CurlFtpAccessDenied
 | CurlFtpUserPasswordIncorrect
 | CurlFtpWeirdPassReply
 | CurlFtpWeirdUserReply
 | CurlFtpWeirdPASVReply
 | CurlFtpWeird227Format
 | CurlFtpCantGetHost
 | CurlFtpCantReconnect
 | CurlFtpCouldnSetBinary
 | CurlPartialFile
 | CurlFtpCouldntRetrFile
 | CurlFtpWriteError
 | CurlFtpQuoteError
 | CurlHttpReturnedError
 | CurlWriteError
 | CurlMalformatError
 | CurlFtpCouldnStorFile
 | CurlReadError
 | CurlOutOfMemory
 | CurlOperationTimeout
 | CurlFtpCouldntSetAscii
 | CurlFtpPortFailed
 | CurlFtpCouldntUseRest
 | CurlFtpCouldntGetSize
 | CurlHttpRangeError
 | CurlHttpPostError
 | CurlSSLConnectError
 | CurlBadDownloadResume
 | CurlFileCouldntReadFile
 | CurlLDAPCannotBind
 | CurlLDPAPSearchFailed
 | CurlLibraryNotFound
 | CurlFunctionNotFound
 | CurlAbortedByCallback
 | CurlBadFunctionArgument
 | CurlBadCallingOrder
 | CurlInterfaceFailed
 | CurlBadPasswordEntered
 | CurlTooManyRedirects
 | CurlUnknownTelnetOption
 | CurlTelnetOptionSyntax
 | CurlObsolete
 | CurlSSLPeerCertificate
 | CurlGotNothing
 | CurlSSLEngineNotFound
 | CurlSSLEngineSetFailed
 | CurlSendError
 | CurlRecvError
 | CurlShareInUse
 | CurlSSLCertProblem
 | CurlSSLCipher
 | CurlSSLCACert
 | CurlBadContentEncoding
 | CurlLDAPInvalidUrl
 | CurlFilesizeExceeded
 | CurlFtpSSLFailed
 | CurlSendFailRewind
 | CurlSSLEngineInitFailed
 | CurlLoginDenied
 | CurlTFtpNotFound
 | CurlTFtpPerm
 | CurlTFtpDiskFull
 | CurlTFtpIllegal
 | CurlTFtpUnknownId
 | CurlTFtpExists
 | CurlTFtpNoSuchUser
 | CurlConvFailed
 | CurlConvReqd
 | CurlSSLCACertBadFile
 | CurlRemoteFileNotFound
 | CurlSSH
 | CurlSSLShutdownFailed
 | CurlAgain
 | CurlSSLCRLBadFile
 | CurlSSLIssuerError
 | CurlFtpPretFailed
 | CurlRtspCseqError
 | CurlRtspSessionError
 | CurlFtpBadFileList
 | CurlChunkFailed
 | CurlNoConnectionAvailable
 | CurlSSLPinnedPubkeyNotMatch
 | CurlSSLInvalidcertstatus
 | CurlHttp2Stream
 | CurlRecursiveApiCall
 | CurlAuthError
 | CurlHttp3
 | CurlQuicConnectError
 | CurlProxyCode
 | CurlSslClientCert
 | CurlUnrecoverablePoll
 | CurlObsolete
   deriving ( Eq, Show, Enum )

toCode :: CInt -> CurlCode
toCode x = toEnum (fromIntegral x)
