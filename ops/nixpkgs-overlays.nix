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
      { src = self.pkgs.fetchgit
        { url = "https://github.com/meejah/txtorcon";
          sha256 = "185sbraqbd209kqnlrq90hv71bi1m758q97ilj8bh99z73c5xf3n";
          rev = "v18.2.0";
        };
      });
    };
  };
  python3Packages = python3.pkgs;
}
