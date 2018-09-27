{ tor, fetchurl }:
tor.overrideAttrs (old:
rec { name = "tor-0.3.5.2-alpha";
      src = fetchurl
      { url = "https://www.torproject.org/dist/tor-0.3.5.2-alpha.tar.gz";
        sha256 = "16pc99sam4lvgnmvb8sh1k89hskrkl1p79wda93pwdcyry5mvmx4";
      };
    }
)
