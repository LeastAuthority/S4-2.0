# https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages
{ pkgs, compiler ? "ghc843", ghc-option ? "" }:
let
  s4 = pkgs.haskell.packages.${compiler}.callPackage ./s4.nix { };
in
  if ghc-option != "" then
    pkgs.haskell.lib.appendBuildFlag s4 "--ghc-option ${ghc-option}"
  else
    s4
