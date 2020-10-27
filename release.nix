{ nixpkgs ? import ./nix/pinnedNix.nix { config = { allowBroken = true; }; } }:
let

  inherit (nixpkgs) pkgs;

in
pkgs.haskell.packages.ghc884.callCabal2nix "hello" ./. { }
