let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        inductive-mazes = hself.callCabal2nix "inductive-mazes" ./. { };

        pure-zlib = self.haskell.lib.dontCheck hsuper.pure-zlib;
      };
    };
  };

  nixpkgs = (import <nixpkgs> { }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "9fa6a261fb237f68071b361a9913ed1742d5e082";
    sha256 = "11733y8xfbisvp8jzpcpjwz70883qfnlzdxv7yl3k2accin88a9z";
  };

  pkgs = import nixpkgs {
    overlays = [ overlay ];
  };

  buildDepends = with pkgs.haskellPackages; [ cabal-install stylish-haskell ];
in
  pkgs.haskell.lib.shellAware (pkgs.haskell.lib.addBuildDepends pkgs.haskellPackages.inductive-mazes buildDepends)
