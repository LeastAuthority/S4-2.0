let
  region = "eu-west-1";
  accessKeyId = "leastauthority-staging";
  zcashnode =
  { config, pkgs, resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    # We need at least 2GB for the Zcash bootstrap process alone.
    deployment.ec2.ebsInitialRootDiskSize = 10;
    deployment.ec2.instanceType = "t3.small";
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.securityGroups = [ "allow_all" ];
  };
in
{
  zcashnode = zcashnode;
  resources.ec2KeyPairs.my-key-pair =
  { inherit region accessKeyId; };
}
