Subscription Configuration
==========================

This document intends to describe the configuration details for an S4 2.0 Tahoe-LAFS client.
It covers both the semantics and the serialization format used when transmitting the configuration via Magic Wormhole.

Negotiation
-----------

The Magic Wormhole interaction involves a negotiation between the client and server to establish compatibility.
The client and server both initially send messages.
The client message must be::

  {"message": {"offer": "{\"abilities\": {\"client-v1\": {}}}"}}

Note that ``message.offer`` is the *JSON serialization* of::

  {"abilities": {"client-v1": {}}}

The server message must be::

  {"message": {"offer": "{\"abilities\": {\"server-v1\": {}}}"}}

Note that ``message.offer`` is the *JSON serialization* of::

  {"abilities": {"server-v1": {}}}

The object associated with `"client-v1"` and `"server-v1"` *may* have additional properties to indicate compatible deviations from this specification.
Clients and servers with such deviations *must* interoperate with peers that follow this specification exactly.
Therefore, clients and servers that follow this specifications *should* ignore any properties on these objects that they do not recognize.

Once the server has received and recognized the client's negotiation message and determined compatibility,
it will send the subscription's first invoice.

The client *must* only process the invoice message if it can determine compatibility given the server's first message.

Invoice
-------

The invoice is a `BIP-72`_\ -inspired URI encoding payment *and* usage details for the subscription.
The details of the invoice structured are `documented elsewhere <invoice.rst>`_.
The **m**\ essage field of the invoice is used to carry the Tahoe-LAFS configuration.
The message field is urlsafe-base64 encoded at the invoice layer.
Decoding that content will produce the Tahoe-LAFS configuration.

The invoice is delivered as a message following the server's negotiation message.
It appears as follows::

  { "version": 2
  , "schema": "https://leastauthority.com/schemas/s4-2.0.schema.json"
  , "invoice": "z..."
  }

The ``version`` field is present to differentiate this message from the earlier exchange implemented by Tahoe-LAFS 1.13 for the ``tahoe invite`` and ``tahoe create-node --join`` UX.
Version ``2`` here indicates there is a ``schema`` property which provides further interpretation information.
The schema for this invoice message **and** for the Tahoe-LAFS configuration in the invoice is as given in the example above.

Configuration
-------------

The configuration message decoded from the invoice is a JSON-format data structure as follows::

  { "shares-needed": 2
  , "shares-happy ": 3
  , "shares-total": 5
  , "nickname": "Least Authority S4 2.0"
  , "storage":
    { "v0-aaaa":
      { "anonymous-storage-FURL": "pb://mmmm@nnnn.onion:12345/oooo"
      }
    , "v0-bbbb":
      { "anonymous-storage-FURL": "pb://pppp@qqqq.onion:12345/rrrr"
      }
    , ...
    }
  }

More formally (`JSON Schema`_)::

  { "$schema": "http://json-schema.org/draft-07/schema#"
  , "$id": "https://leastauthority.com/schemas/s4-tahoe-config-2.0.schema.json"
  , "title": "Tahoe-LAFS Configuration"
  , "description": "Tahoe-LAFS Configuration"
  , "type": "object"
  , "properties":
    { "shares-needed": { "type": "integer" }
    , "shares-happy": { "type": "integer" }
    , "shares-total": { "type": "integer" }
    , "nickname": { "type": "string" }
    , "storage":
      { "type": "object"
      , "properties":
	{ "anonymous-storage-FURL": { type": "string" }
	}
      , "required": [ "anonymous-storage-FURL" ]
      }
    }
  , "required": [ "shares-needed", "shares-happy", "shares-total", "storage", "nickname" ]
  }


.. _JSON Schema: https://json-schema.org/
.. _BIP-72: https://github.com/bitcoin/bips/blob/master/bip-0072.mediawiki
