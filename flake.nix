{
  description = "aoc-2022";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        myHaskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: { };
        };

        haskellDeps = ps: with ps; [
          relude
          megaparsec
          lens
          generic-lens
          advent-of-code-api
          recursion-schemes
          containers
          matrix
          fgl
        ];
        my-ghc = myHaskellPackages.ghcWithPackages haskellDeps;

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            my-ghc
            myHaskellPackages.haskell-language-server
            myHaskellPackages.ormolu
          ];
          shellHook = ''
            export ADVENT_OF_CODE_TOKEN=`cat .aoctoken`;
          '';
        };
      }
    );
}
