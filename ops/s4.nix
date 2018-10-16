# Describe the software to run on the infrastructure described by s4-ec2.nix.
let
  overlays = [ (import ./nixpkgs-overlays.nix) ];
  services = (import <nixpkgs> { inherit overlays; }).callPackage ./services.nix { };
  tor = import ./tor.nix;
  nginx = import ./nginx.nix;
  zcashUser =
  { isNormalUser = true;
    home = "/var/lib/zcashd";
    description = "Runs a full Zcash node";
  };
  zcashPorts = [ 18232 18233 ];
  zcashPackages = pkgs:
  [ pkgs.zcash
  ];
in
{ network.description = "S4-2.0";

  defaults = { ... }:
  {
    # Arrange for some packages to be overridden with different versions on
    # all machines.
    nixpkgs.overlays = overlays;
  };

  # The lockbox node runs a Zcash node and contains the master keys for
  # payment addresses used by the service.  That is, this is the node with the
  # authority to spend money earned by the service.  It is kept minimal and is
  # expected to be deployed in a way that is consistent with this authority
  # (specifically with respect to its physical and operational security).
  lockbox =
  { lib, pkgs, ... }:
  { networking.firewall.allowedTCPPorts = zcashPorts;
    users.users.zcash = zcashUser;
    environment.systemPackages = zcashPackages pkgs;
    systemd.services.zcashd = services.zcashdService;
  };

  # The infrastructure node runs various "fixed overhead" services.  For
  # example, a Zcash node for observing incoming payments and a Tor daemon for
  # providing Onion access to the Least Authority website.  These services are
  # not expected to need to scale horizontally.  They may eventually need to
  # be made redundant somehow to ensure availability.
  infra =
  { lib, pkgs, ... }:
  let s4signupwebsite = pkgs.callPackage ./s4signupwebsite.nix { };
      torControlPort = 9051;
      mainWebsiteProxyPort = 9061;
  in
  # Allow the two Zcash protocol ports.
  { networking.firewall.allowedTCPPorts = zcashPorts;
    users.users.zcash = zcashUser;
    systemd.services.zcashd = services.zcashdService;
    environment.systemPackages = zcashPackages pkgs;

    /*
     * Nix-generated Tor configuration file has a `User tor` that we cannot
     * easily control.  This option sets the UID *and* GID of the Tor process.
     * The keys we want to pass to Tor for the website configuration are in
     * /run/keys which is group-readable and group-owned by `keys`.  So, get
     * the tor user's primary group to be the keys group so it can actually
     * read the keys.
     *
     * We have to force this value to override the previously supplied value.
     *
     * Note that this means the tor user no longer belongs to the tor group.
     * This may break something but if it does I haven't yet observed it.
     *
     * This is kind of lame, I think.  Maybe it would be better not to have
     * keys on the disk or something?
     */
    users.users.tor.group = lib.mkForce "keys";

    /*
     * Run a Tor node so we can operate a hidden service to allow user signup.
     */
    services.tor.enable = true;

    /*
     * Enable the control port so that we can do interesting things with the
     * daemon from other programs.
     */
    services.tor.controlPort = torControlPort;
    services.tor.extraConfig = tor.extraConfig
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
      # We don't make outgoing Tor connections via the SOCKS proxy.  Disable
      # it.  This is also necessary to use HiddenServiceNonAnonymousMode.
      #
      # NixOS Tor support defaults to disabling "client" features but there
      # seems to be a bug where it leaves the Socks server enabled anyway.
      socksPort = 0;
    };

    /*
     * Customize the Tor service so that it is able to read the keys with which
     * we will supply it.
     */
    systemd.services.tor =
    { serviceConfig =
      { ReadWritePaths =
        [ # Let it keep track of its various internal state.
          "/var/lib/tor"
        ];
      };
    };

    /* Provide a private key for the website Onion service. */
    /* https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html */
    /* https://nixos.org/nixops/manual/#idm140737318276736 */
    deployment.keys."signup-website-tor-onion-service-v3.secret" =
    { keyFile = ./secrets/onion-services/v3/signup-website.secret;
      user = "tor";
      group = "tor";
      permissions = "0600";
    };
    # And the key for the Onion-ified main website.
    deployment.keys."main-website-tor-onion-service-v3.secret" =
    { keyFile = ./secrets/onion-services/v3/main-website.secret;
      user = "tor";
      group = "tor";
      permissions = "0600";
    };

    /*
     * Operate a static website allowing user signup, exposed via the Tor
     * hidden service.
     */
   systemd.services."signup-website" =
   let keyService = "signup-website-tor-onion-service-v3.secret-key.service";
       # Run an Onion-to-HTTPS port forward to expose the website at an Onion
       # service address.
       script =
       ''
       twist --log-format=text web \
             --path ${s4signupwebsite} \
             --port onion:public_port=80:controlPort=${toString torControlPort}:privateKeyFile=/run/keys/signup-website-tor-onion-service-v3.secret:singleHop=true
       '';
       description = "The S4 2.0 signup website.";
   in
   services.txtorconService keyService script // { inherit description; };

    # Create a local-only nginx proxy server that will accept cleartext
    # requests (received via an Onion service, typically) and proxy them to
    # the HTTPS main website server.
    services.nginx = nginx.cleartextToHTTPSProxy
      pkgs
      "leastauthority-main-website"
      { proxyPass = "https://leastauthority.com/";
        listen = [ { addr = "127.0.0.1"; port = mainWebsiteProxyPort; } ];
        serverName = "leastauthority.com";
      };

    # Create an Onion service that proxies cleartext connections to the local
    # cleartext nginx proxy server.
    systemd.services."main-website" =
    let keyService = "main-website-tor-onion-service-v3.secret-key.service";
        script =
        ''
        twist --log-format=text portforward \
              --port onion:public_port=80:controlPort=${toString torControlPort}:privateKeyFile=/run/keys/main-website-tor-onion-service-v3.secret:singleHop=true \
              --dest_port ${toString mainWebsiteProxyPort}
        '';
        description = "The overall Least Authority website with all content.";
    in
    services.txtorconService keyService script // { inherit description; };
  };

}
