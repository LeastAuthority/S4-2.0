S4 2.0
======

This is the DevOps stuff to deploy and operate S4 2.0.

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
