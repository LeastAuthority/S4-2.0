{
  zcashnode =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 8192; # megabytes
      deployment.virtualbox.vcpu = 1; # number of cpus
    };
}
