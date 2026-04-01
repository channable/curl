{ mkDerivation, base, bytestring, text, containers, curlFull, lib }:
# We require a minimum version of libcurl to ensure that our bindings are sufficiently
# compatible.
assert lib.versionAtLeast curlFull.version "8.18.0";
mkDerivation {
  pname = "curl";
  version = "1.3.8";
  src = lib.cleanSource ./.;
  isExecutable = false;
  isLibrary = true;
  libraryHaskellDepends = [ base bytestring containers text ];
  librarySystemDepends = [ curlFull ];
  description = "Haskell binding to libcurl";
  license = lib.licenses.bsd3;
}
