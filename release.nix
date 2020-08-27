{ compiler ? "ghc865" }:

let

  # pkgs = import <nixpkgs> { };
  pkgs = import ./nix/source.nix { json = ./nix/source.json; };

in
  pkgs.haskell.packages.${compiler}.callCabal2nix "hello" ./. {}

