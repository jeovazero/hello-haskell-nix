{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; }
 }:
let
  inherit (nixpkgs) pkgs;

  project = pkgs.haskell.lib.justStaticExecutables
              ( pkgs.haskellPackages.callCabal2nix "hello" ./. { } );
in

pkgs.dockerTools.buildImage {
  name = "hello-tools-api";
  created = "now";
  config = {
    Cmd = [ "${project}/bin/hello" ];
  };
}
