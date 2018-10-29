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
it will send the configuration message.

The client *must* only process the configuration message if it can determine compatibility given the server's first message.

Configuration
-------------

The configuration message which follows the server's negotiation message is as follows::

  { "sharesNeeded": 2
  , "sharesHappy ": 3
  , "sharesTotal": 5
  , "storageFURLs": [ "pb://xxxx@yyyy.onion:12345/zzzz", ... ]
  , "nickname": "Least Authority S4 2.0"
  }

More formally (`JSON Schema`_)::

  { "$schema": "http://json-schema.org/draft-07/schema#"
  , "$id": "https://leastauthority.com/schemas/s4-2.0.schema.json"
  , "title": "Tahoe-LAFS Configuration"
  , "description": "Tahoe-LAFS Configuration"
  , "type": "object"
  , "properties":
    { "sharesNeeded": { "type": "integer" }
    , "sharesHappy": { "type": "integer" }
    , "sharesTotal": { "type": "integer" }
    , "storageFURLs": { "type": "array", "items": { "type": "string" } }
    , "nickname": { "type": "string" }
    }
  , "required": [ "sharesNeeded", "sharesHappy", "sharesTotal", "storageFURLs", "nickname" ]
  }


.. _JSON Schema: https://json-schema.org/
