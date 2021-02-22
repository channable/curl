{ mkDerivation, base, bytestring, containers, curlFull, lib, stdenv, hpack }:
assert lib.versionAtLeast curlFull.version "7.17.0";
mkDerivation {
  pname = "curl";
  version = "1.3.8";
  src = stdenv.lib.cleanSource ./.;
  isExecutable = false;
  isLibrary = true;
  libraryHaskellDepends = [ base bytestring containers ];
  librarySystemDepends = [ curlFull ];
  description = "Haskell binding to libcurl";
  license = stdenv.lib.licenses.bsd3;
}
