# Tor-related helpers.
let
  torBool = x: if x then "1" else "0";
in
rec {
  # Construct an extra Tor configuration blob to insert into the Tor
  # configuration file.
  extraConfig =
  { cookieAuthentication, hiddenServiceSingleHopMode, hiddenServiceNonAnonymousMode }:
  ''
  CookieAuthentication ${torBool cookieAuthentication}
  HiddenServiceSingleHopMode ${torBool hiddenServiceSingleHopMode}
  HiddenServiceNonAnonymousMode ${torBool hiddenServiceNonAnonymousMode}
  '';

  torService = controlPort:
  { enable = true;
    /*
     * Enable the control port so that we can do interesting things with the
     * daemon from other programs.
     */
    inherit controlPort;

    # We don't make outgoing Tor connections via the SOCKS proxy.  Disable
    # this feature.  This is also required by HiddenServiceNonAnonymousMode.
    #
    # If we merely tell NixOS to disable the client entirely it won't write
    # out the necessary `SocksPort` configuration item.  So spell out the
    # exact configuration we need ourselves.
    #
    # See https://github.com/NixOS/nixpkgs/pull/48625
    client =
    { enable = true;
      socksListenAddress = "0";
      socksListenAddressFaster = "0";
      socksIsolationOptions = [];
    };

    extraConfig = extraConfig
    {
      # Enable authentication on the ControlPort via a secret shared cookie.
      cookieAuthentication = true;

      # Quoting https://gitweb.torproject.org/torspec.git/tree/control-spec.txt
      #
      # Tor instances can either be in anonymous hidden service mode, or
      # non-anonymous single onion service mode. All hidden services on the
      # same tor instance have the same anonymity. To guard against unexpected
      # loss of anonymity, Tor checks that the ADD_ONION "NonAnonymous" flag
      # matches the current hidden service anonymity mode. The hidden service
      # anonymity mode is configured using the Tor options
      # HiddenServiceSingleHopMode and HiddenServiceNonAnonymousMode. If both
      # these options are 1, the "NonAnonymous" flag must be provided to
      # ADD_ONION. If both these options are 0 (the Tor default), the flag
      # must NOT be provided.
      #
      # Our purpose in using Tor is not to protect our location privacy
      # ("anonymity") but to ensure that clients have their location privacy
      # protected.  Therefore, we can drop the Tor behaviors which are for
      # protecting our location privacy.  This should offer some amount of
      # performance improvement as well due to the reduced number of hops to
      # reach the service.
      hiddenServiceSingleHopMode = true;
      hiddenServiceNonAnonymousMode = true;
    };
  };
}
