{ nixpkgs ? import ./nix/pinnedNix.nix { }
}:
let
  inherit (nixpkgs) pkgs;

  project = pkgs.haskell.lib.justStaticExecutables
    (import ./release.nix {});
in
pkgs.dockerTools.buildImage {
  name = "hello-tools-api";
  created = "now";
  config = {
    Cmd = [ "${project}/bin/hello" ];
  };
}
