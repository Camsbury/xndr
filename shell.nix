let
  nixPinned = import (builtins.fetchTarball  "https://github.com/NixOS/nixpkgs/archive/fa82ebccf66.tar.gz") {};
in
  { nixpkgs ? nixPinned }:
    let
      xndr = (import ./default.nix { inherit nixpkgs; });
      xndrShell = with nixpkgs;
      haskell.lib.overrideCabal xndr (oldAttrs: {
        librarySystemDepends = with pkgs; [
          cabal-install
          haskellPackages.ghcid
        ];
      });
    in
      xndrShell.env
