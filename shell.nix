{ pkgs ? import <nixpkgs> { } }:
let
  ghc = pkgs.ghc;
  project = pkgs.haskellPackages.callPackage ./nix/default.nix { };
  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc883;

in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    ghcide
    haskellPackages.cabal2nix
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.cabal2nix
    haskellPackages.ghcid
    coreutils
  ];
  shellHook = ''
    export NIX_GHC="`which ghc`"
    export NIX_GHCPKG="`which ghc`/../ghc-pkg"
    export NIX_GHC_DOCDIR="`which ghc`/../../share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
