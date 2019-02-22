let
  nixPinned =
  import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/fa82ebccf66.tar.gz") {};
in
  { nixpkgs ? nixPinned }:
  nixpkgs.pkgs.haskellPackages.callCabal2nix "xndr" ./. {}
