{
  description = "Toolchains";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskellPackages;
      in
      {
        packages.default = pkgs.haskell.lib.justStaticExecutables (
          haskell.callCabal2nix "planning-poker" ./. { }
        );

        devShells.default = pkgs.mkShell {
          packages = with haskell; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            cabal-fmt
          ];

          buildInputs = [
            pkgs.zlib
          ];
        };
      }
    );
}
