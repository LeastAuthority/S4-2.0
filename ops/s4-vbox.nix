{ bridgeAdapter }:
{ infra =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 8192; # megabytes
      deployment.virtualbox.vcpu = 1; # number of cpus
      deployment.virtualbox.headless = true;
      deployment.virtualbox.vmFlags =
      [ "--nic1" "bridged" "--bridgeadapter1" bridgeAdapter
      ];
    };
}
