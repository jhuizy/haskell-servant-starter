{ pkgs ? import <nixpkgs> { } }:
let
  project = pkgs.haskellPackages.callPackage ./default.nix { };

in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcide
    haskellPackages.cabal2nix
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.cabal2nix
    haskellPackages.ghcid
  ];
}
