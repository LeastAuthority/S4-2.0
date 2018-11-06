# https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages
{ pkgs, compiler ? "ghc843", get-hspec-jenkins ? (pkgs: ghc: ghc.hspec-jenkins) }:
let
  ghc = pkgs.haskell.packages.${compiler};
  hspec-jenkins = get-hspec-jenkins pkgs ghc;
  s4 = ghc.callPackage ./s4.nix { };
in
  pkgs.haskell.lib.overrideCabal s4 (old:
  { pname = "hspec-jenkins-lae"; testHaskellDepends = old.testHaskellDepends ++ [ hspec-jenkins ];
  })
