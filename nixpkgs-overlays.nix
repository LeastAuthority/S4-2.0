self: super:
rec {
  haskell = super.haskell //
  { packageOverrides = ghc-self: ghc-super:
    { # Make our buildable fork of this available.
      hspec-jenkins = super.callPackage ./hspec-jenkins-lae { };
    };
  };

  s4 = super.callPackage ./s4 { };
}
