{ nixpkgs ? import <nixpkgs> {} }:
let
  xndrBare = nixpkgs.pkgs.haskellPackages.callCabal2nix "xndr" ./. {};
in
  nixpkgs.haskell.lib.overrideCabal xndrBare ( oldAttrs: {
    librarySystemDepends = with nixpkgs.pkgs; [cabal-install ghc];
  })
