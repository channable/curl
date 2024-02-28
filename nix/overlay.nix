final: previous:
{
  curl_channable = {
    haskellOverlay = final.callPackage ./haskell-overlay.nix {};

    haskellPackages =
      previous.haskell.packages.ghc94.extend final.curl_channable.haskellOverlay;

    staticHaskellPackages =
      previous.pkgsStatic.haskell.packages.ghc94.extend final.curl_channable.haskellOverlay;
  };
}
