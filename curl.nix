{ mkDerivation, base, bytestring, containers, curlFull, stdenv, hpack }:
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
