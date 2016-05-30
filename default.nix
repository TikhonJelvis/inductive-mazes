{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cairo, fgl, MonadRandom, stdenv }:
      mkDerivation {
        pname = "inductive-mazes";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base cairo fgl MonadRandom ];
        homepage = "http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/";
        description = "A simple library for generating mazes using inductive graphs from fgl (the \"functional graph library\")";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
