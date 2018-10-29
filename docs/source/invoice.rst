Invoices
========

The resource identified by the invoice is a Zcash shielded address to which payment must be issued.
The parameters of the invoice are as follows:

=== ======== =====
Key Required Value
=== ======== =====
 c    yes    Cybercurrency name (eg "ZEC")
 a    yes    Amount (eg "0.1")
 d    yes    The date due of the subscription period, as ISO 8601
 e    yes    The extension date of the subscription, as ISO 8601
 l    yes    The label for the service, urlsafe-base64 encoded
 u    yes    The URL to for the next invoice, urlsafe-base64 encoded
 m    yes    The Tahoe-LAFS configuration, urlsafe-base64 encoded
 r    no     Credit currently applied to the account as a result of a partial payment or an overpayment of a previous invoice.
             A payment of a - r is required for this invoice to be paid in full.
 v    no     A version identifier. Assumed to be “1” if omitted.
 p    yes    The Ed25519 pubkey with which the next invoice will be signed, urlsafe-base64 encoded
 s    yes    The Ed25519 signature of the current invoice string.
=== ======== =====

The **v**\ ersion field is intended to specify interpretation rules for the invoice resource and the parameters.
For example,
if the urlsafe-base64 encoding rules change then they will do so along with a version increment.
