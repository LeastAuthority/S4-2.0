S4 2.0
======

This is the DevOps stuff to deploy and operate S4 2.0.

Quick Start
~~~~~~~~~~~

VirtualBox Deployment
---------------------

Creating a deployment running entirely within local VirtualBox VMs is an easy way to tinker with S4 2.0.
At least, it is in principle.
In practice, the interaction between NixOps and VirtualBox seems fragile.
There is a good chance this won't actually work for you.
It doesn't work for me.
Bearing that in mind ...

With a working directory of the root of a checkout of S4-2.0::

   nixops create --deployment your-s4-petname ops/s4.nix ops/s4-vbox.nix
   nixops deploy --deployment your-s4-petname

AWS Deployment
--------------

Set ``AWS_PROFILE`` in your environment.
If the AWS account selected has never previously been initialized for S4 2.0 then,
with the root of your S4-2.0 checkout as your working directory::

   terraform init ops
   terraform apply ops

Then, with the same working directory::

   nixops create --deployment your-s4-petname ops/s4.nix ops/s4-ec2.nix
   nixops deploy --deployment your-s4-petname

Tools
~~~~~

Terraform
---------

Terraform is used to manage certain resources.
The Terraform configuration lives in ``.tf`` files.
It is typically applied using ``terraform apply``.

NixOps
------

As much of the deployment as possible is defined using NixOps.
The NixOps configuration lives in ``.nix`` files.
It is typically applied using ``nixops deploy --deployment s4``.

Components
~~~~~~~~~~

Zcash
-----

A non-mining Zcash full node is included as part of this deployment.
This allows for processing of Zcash shielded transactions.
The Zcash deployment is primarily configured using ``zcashnode`` in ``s4.nix``.

Tor
---

A Tor daemon is included as part of this deployment.
This allows the service website and Tahoe-LAFS daemons to operate as Onion services.
This, in turn, provides location privacy for users browsing the website and operating Tahoe-LAFS clients.
The Tor node is primary configured using ``zcashnode`` in ``s4.nix``
(but maybe this should change).

Tor Service Keys
````````````````

To publish the website at a stable Onion service address,
the deployment requires use of persistent private keys.
No such keys have yet been provisioned.
Meanwhile, you can generate some throw-away keys::

  pip install stem
  mkdir -p ops/secrets/onion-services/v3
  ./bin/generate-onion-keys ops/secrets/onion-services/v3/signup-website
  ./bin/generate-onion-keys ops/secrets/onion-services/v3/main-website

You must have keys before you can use ``nixops`` to deploy the service.
