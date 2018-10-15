{ ... }:
{
  txtorconService = pkgs: keyService: script:
  { unitConfig.Documentation = "https://leastauthority.com/";

    path = [ (pkgs.python3.withPackages (ps: [ ps.twisted ps.txtorcon ])) ];

    # Get it to start as a part of the normal boot process.
    wantedBy    = [ "multi-user.target" ];

    # Make sure Tor is up and our keys are available.
    # https://www.freedesktop.org/software/systemd/man/systemd.unit.html#Requires=
    requires = [ "tor.service" keyService ];
    # https://www.freedesktop.org/software/systemd/man/systemd.unit.html#Before=
    after = [ "tor.service" keyService ];

    inherit script;
  };
  zcashdService = pkgs:
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
    # Get it to start as a part of the normal boot process.
    wantedBy    = [ "multi-user.target" ];

    # Make sure we have network connectivity before bothering to try to
    # start zcashd.
    requires = [ "network-online.target" ];

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
      # ReadWriteDirectories    = "${pkgs.zcash}/bin /var/lib/zcashd";
      # InaccessibleDirectories = "/home";
      StateDirectory          = "zcashd";

      # Parameters are required before a node can start.  These are fetched
      # from the network.  This only needs to happen once.  Currently we try
      # to do it every time we're about to start the node.  Maybe this can
      # be improved.
      ExecStartPre            = "${pkgs.zcash}/bin/zcash-fetch-params";

      # Rely on $HOME to set the location of most Zcashd inputs.  The
      # configuration file is an exception as it lives in the store.
      ExecStart               = "${pkgs.zcash}/bin/zcashd -conf=${conf}";
    };
  };
}
