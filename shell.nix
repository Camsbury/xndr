let
  nixPinned = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/18.09-beta.tar.gz";
    sha256 = "147xyn8brvkfgz1z3jbk13w00h6camnf6z0bz0r21g9pn1vv7sb0";
  }) {};
in
  { nixpkgs ? nixPinned }:
    (import ./default.nix { inherit nixpkgs; }).env
