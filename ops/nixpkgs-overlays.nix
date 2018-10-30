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
    { txtorcon = py-super.txtorcon.overrideAttrs(old:
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

  zcash = super.callPackage ./zcash/default.nix { };

  s4 = super.callPackage ../s4/default.nix { };
}
