let
  pinnedPkgs = builtins.fetchTarball {
    name = "nixos-21.05-2021-09-05";
    url = "https://github.com/nixos/nixpkgs/archive/6bfe71f2a4e2e425dee26b25d2309f341ff1600d.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1mpf700fqlzyj6vsy2c329zlgbk9g6giwiyb2g0yhc0a78h72g8l";
  };
 in import pinnedPkgs
