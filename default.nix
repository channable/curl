{ mkDerivation, base, bytestring, containers, curlFull, lib }:
# This is the version of curlFull present in 23.05 on 31/08/2023.
assert lib.versionAtLeast curlFull.version "8.1.1";
mkDerivation {
  pname = "curl";
  version = "1.3.8";
  src = lib.cleanSource ./.;
  isExecutable = false;
  isLibrary = true;
  libraryHaskellDepends = [ base bytestring containers ];
  librarySystemDepends = [ curlFull ];
  description = "Haskell binding to libcurl";
  license = lib.licenses.bsd3;
}
