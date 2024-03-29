let

  nixpkgs = import ./nix/pinnedNix.nix { };

  inherit (nixpkgs) pkgs;

  inherit (pkgs)
    haskell sqitchPg postgresql_12 cabal2nix hlint ghcid cabal-install arion
    stylish-haskell;

  haskellPackages = haskell.packages.ghc8104;

  project = import ./release.nix { };
in pkgs.stdenv.mkDerivation {
  name = "shell";

  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    ghcid
    hlint
    arion
    sqitchPg
    postgresql_12
    cabal2nix
    stylish-haskell
  ];

  # https://github.com/NixOS/nix/issues/599
  LOCALE_ARCHIVE = "/usr/lib/locale/locale-archive";

  # https://github.com/mpickering/hie-bios/issues/25#issuecomment-537718071
  shellHook = ''
    export  NIX_GHC="$(which ghc)"
    export  NIX_GHCPKG="$(which ghc-pkg)"
    export  NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
    export  NIX_GHC_LIBDIR="$(ghc --print-libdir)"
  '';
}
