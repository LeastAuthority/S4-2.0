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
Bearing that in mind ...

With a working directory of the root of a checkout of S4-2.0::

   nixops create --deployment your-s4-petname ops/s4.nix ops/s4-vbox.nix
   nixops set-args --arg bridgeAdapter '"<host network interface>"'
   nixops deploy --force-reboot --deployment your-s4-petname

For ``<host network interface>``,
select a network interface on the host which can route traffic out of your network
and which has a DHCP server.
This might be something like ``wlp4s0`` or ``enp0s31f6``.
Make sure to include all of the quotes as indicated above.

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

Nix Tools
---------

Nix is a collection of tools for managing a purely functional Linux distribution.

As much of the deployment as possible is managed using the Nix tools.
The configuration lives in ``.nix`` files in this repository.
It is typically applied using ``nixops deploy --deployment s4``.

NixOS
`````

`NixOS`_ is a purely functional Linux distribution.
Compared to other Linux distributions,
NixOS has the advantages of declarative system configuration,
atomic upgrades and rollbacks,
and a tailor-made operational management tool (`NixOps`_).

Nix
```

`Nix`_ is a purely functional package manager.
It is used to configure a NixOS installation.
It is also a purely functional programming language in which this configuration is written.

NixOps
``````

`NixOps`_ is a tool for deploying sets of `NixOS`_ Linux machines.
It uses and extends the purely functional `Nix`_ package manager to allow node provisioning and network configuration.

Nixpkgs
```````

`Nixpkgs`_ is a collection of packages available for NixOS.
Many of these are used in the deployment.
NixOS is not limited to the packages in this collection.
Where we have needs that go beyond Nixpkgs we can use Nix to create our own packages.

Components
~~~~~~~~~~

Zcash
-----

Multiple non-mining Zcash Sapling full nodes are included as part of this deployment.
This allows for processing of Zcash shielded transactions.
The Zcash deployment is configured in ``ops/s4.nix`` via ``zcashdService``.

A **lockbox** node is provisioned for maximum resistance to key compromise and holds spending keys.
An **infra** node is provisioned for off-premise deployment and holds only viewing keys.

Wallet
``````

z-addr keys are required to process incoming transactions representing subscription payments.
The **infra** wallet contains *only* the viewing keys necessary for this processing.
This limits the damage that can be done should the wallet be compromised
(transactions can be read but funds cannot be spent).

The master spending key(s) are maintained on the **lockbox** node.
Each subscription is assigned a unique `Sprout z-addr<https://github.com/LeastAuthority/S4-2.0/issues/38>`_.
As new subscriptions are created and addresses available on the node are allocated,
the "pool" of addresses is diminished.
Before the pool empties,
new viewing keys are generated in the **lockbox** wallet and imported into the **infra** wallet.

``bin/load-some-more-keys`` automates this process of key creation and movement.

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

.. _NixOps: https://nixos.org/nixops/
.. _NixOS: https://nixos.org/
.. _Nix: https://nixos.org/nix/
.. _Nixpkgs: https://nixos.org/nixpkgs/
