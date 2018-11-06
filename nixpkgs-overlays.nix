self: super:
rec {
  python2 = super.python2.override
  { packageOverrides = py-self: py-super:
    { spake2 = py-super.spake2.overrideAttrs (old: rec
      { # Upstream spake2 check phase leaves pytest garbage lying around that
        # results in file collisions.
        postCheck = ''
        rm -rvf $out/.pytest_cache
        '';
        # And then it re-runs the tests in the "install check" phase.  Just
        # disable that test run.  It seems like it should be possible to do
        # (or re-do) the cleanup in postInstallCheck but postInstallCheck
        # doesn't seem to get run (I don't know why).
        doInstallCheck = false;
      });
    };
  };
  python2Packages = python2.pkgs;

  haskell = super.haskell //
  { packageOverrides = ghc-self: ghc-super:
      # Provide some additional Python bits and bops required by the Spake2 and Magic-Wormhole test suites.
    { spake2 = ghc-super.spake2.overrideAttrs (old:
      { buildInputs = old.buildInputs ++ [ (self.python2.withPackages (ps: [ ps.attrs ps.spake2 ps.hkdf ])) ];
      });
      magic-wormhole = ghc-super.magic-wormhole.overrideAttrs (old:
      { buildInputs = old.buildInputs ++ [ (self.python2.withPackages (ps: with ps;
      [ spake2 pynacl attrs twisted autobahn automat hkdf tqdm click humanize txtorcon magic-wormhole ])) ];
      });

      # Make our buildable fork of this available.
      hspec-jenkins = super.callPackage ./hspec-jenkins-lae { };
    };
  };

  s4 = super.callPackage ../s4 { };
}
