# https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages
{ pkgs, compiler ? "ghc843" }:
pkgs.haskell.packages.${compiler}.callPackage ./s4.nix { }
