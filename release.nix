{ nixpkgs ? import ./nix/pinnedNix.nix { } }:
let
  inherit (nixpkgs) pkgs;
  haskellPkgs = pkgs.haskell.packages.ghc8104;
in
  haskellPkgs.callCabal2nix "hello" ./. { }
