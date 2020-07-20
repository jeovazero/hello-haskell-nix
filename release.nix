{ compiler ? "ghc865" }:

let

  # pkgs = import <nixpkgs> { };
  pkgs = import ./source.nix { json = ./source.json; };

in
  pkgs.haskell.packages.${compiler}.callPackage ./default.nix {}

