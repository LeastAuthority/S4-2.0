{
  network.description = "Zcash server";
  zcashnode =
  { config, pkgs, ... }:
  { networking.firewall.allowedTCPPorts = [ 18232 18233 ];

    users.users.zcash =
    { isNormalUser = true;
      home = "/var/lib/zcashd";
      description = "Runs a full Zcash node";
    };

    environment.systemPackages = [
      # Nix-packaged Zcash distribution - 1.x.
      pkgs.altcoins.zcash
      # Provides flock, required by zcash-fetch-params.  Probably a Nix Zcash
      # package bug that we have to specify it.
      pkgs.utillinux
      # Also required by zcash-fetch-params.
      pkgs.wget
    ];

    systemd.services.zcashd =
      # Write the Zcashd configuration file and remember where it is for
      # later.
      let conf = pkgs.writeText "zcash.conf"
      ''
      # Operate on the test network while we're in development.
      testnet=1
      addnode=testnet.z.cash

      # Don't be a miner.
      gen=0
    '';
    in
    { unitConfig.Documentation = "https://z.cash/";
      description = "Zcashd running a non-mining Zcash full node";
      wantedBy    = [ "multi-user.target" ];

      # Get zcash-fetch-params dependencies into its PATH.
      path = [ pkgs.utillinux pkgs.wget ];

      serviceConfig = {
        Restart                 = "on-failure";
        User                    = "zcash";
        # Nice                    = 19;
        # IOSchedulingClass       = "idle";
        PrivateTmp              = "yes";
        # PrivateNetwork          = "yes";
        # NoNewPrivileges         = "yes";
        # ReadWriteDirectories    = "${pkgs.altcoins.zcash}/bin /var/lib/zcashd";
        # InaccessibleDirectories = "/home";
        StateDirectory          = "zcashd";

        # Parameters are required before a node can start.  These are fetched
        # from the network.  This only needs to happen once.  Currently we try
        # to do it every time we're about to start the node.  Maybe this can
        # be improved.
        ExecStartPre            = "${pkgs.altcoins.zcash}/bin/zcash-fetch-params";

        # Rely on $HOME to set the location of most things.  The configuration
        # file is an exception as it lives in the store.
        ExecStart               = "${pkgs.altcoins.zcash}/bin/zcashd -conf=${conf}";
      };
    };
  };
}
