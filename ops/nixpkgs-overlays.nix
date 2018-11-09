self: super:
rec {
  # Supply a newer version of Tor than is currently available in nixpkgs.
  # This is necessary to make restarting ephemeral Onion services reliable.
  # Old versions of Tor sometimes fail to re-upload the descriptor or even if
  # they succeed in re-uploading it the service remains mysteriously
  # unavailable.
  tor = super.tor.overrideAttrs (old:
    rec { name = "tor-0.3.5.2-alpha";
          src = super.fetchurl
          { url = "https://www.torproject.org/dist/tor-0.3.5.2-alpha.tar.gz";
            sha256 = "16pc99sam4lvgnmvb8sh1k89hskrkl1p79wda93pwdcyry5mvmx4";
          };
        }
  );

  python3 = super.python3.override
  { packageOverrides = py-self: py-super:
    { txtorcon = py-super.txtorcon.overrideAttrs (old:
      { version = "18.3.0";
        name = "txtorcon-18.3.0";
        src = self.pkgs.fetchgit
        { url = "https://github.com/meejah/txtorcon";
          sha256 = "1n4ladjy8sx2x6vydpyx6c37glfh49n5a8vk91aclnw635i2dq7m";
          rev = "v18.3.0";
        };
      });
    };
  };
  python3Packages = python3.pkgs;

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

  zcash = super.callPackage ./zcash/default.nix { };

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
