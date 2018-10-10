# Describe an AWS EC2 instance which can run S4 2.0.  This is expected to be
# replaced by a lower-cost provider later.  Hopefully that change will only
# affect this part of the deployment and all of the software definitions
# (s4.nix) will remain the same.
let
  region = "eu-west-1";
  accessKeyId = "leastauthority-staging";
  infra =
  { config, pkgs, resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    # We need at least 2GB for the Zcash bootstrap process alone.
    deployment.ec2.ebsInitialRootDiskSize = 10;
    deployment.ec2.instanceType = "t3.medium";
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.securityGroups = [ "allow_all" ];
  };
  devnode =
  { config, pkgs, resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    # We need at least 2GB for the Zcash bootstrap process alone.
    deployment.ec2.ebsInitialRootDiskSize = 50;
    deployment.ec2.instanceType = "t3.large";
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.securityGroups = [ "allow_all" ];
};
in
{
  devnode = devnode;
  infra = infra;
  resources.ec2KeyPairs.my-key-pair =
  { inherit region accessKeyId; };
}
