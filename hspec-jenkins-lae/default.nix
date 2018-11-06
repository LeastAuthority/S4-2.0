{ pkgs ? import <nixpkgs> { } }:
pkgs.haskellPackages.callPackage ./hspec-jenkins-lae.nix { }
