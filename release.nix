{ nixpkgs ? import ./nix/pinnedNix.nix { config = { allowBroken = true; }; } }:
let

  inherit (nixpkgs) pkgs;

  ghc884 = pkgs.haskell.packages.ghc884;
  dontCheck = pkgs.haskell.lib.dontCheck;

  password-types = dontCheck (ghc884.callPackage ./nix/password-types.nix {});
  password = ghc884.callPackage ./nix/password.nix { inherit password-types; };

  myPkgs = ghc884.override {
    overrides = self: super: {
      password = dontCheck password;
    };
  };

in
myPkgs.callCabal2nix "hello" ./. { }
