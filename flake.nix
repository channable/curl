{
  description = "Haskell bindings for curl";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils}: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.${system}.curl_channable ];
      };
    in
    {
      overlays.curl_channable = (import ./nix/overlay.nix);

      devShell = with pkgs.curl_channable.haskellPackages; shellFor {
        packages = p: [ p.curl_channable ];
        buildInputs = [
          cabal-install
          cabal-fmt
          haskell-language-server
        ];
      };
    }
  );
}
