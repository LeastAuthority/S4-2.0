from binascii import b2a_base64
from zope.interface import implementer
from twisted.plugin import IPlugin
from twisted.internet.interfaces import IStreamServerEndpointStringParser
from twisted.internet.endpoints import clientFromString

from txtorcon.endpoints import (
    TCPHiddenServiceEndpoint,
)

@implementer(IStreamServerEndpointStringParser, IPlugin)
class TCPHiddenServiceEndpointParser(object):
    prefix = "x-onion"

    def parseStreamServer(
            self,
            reactor,
            public_port,
            privateKeyPath,
            localPort=None,
            controlPort=None,
    ):
        with file(privateKeyPath) as privateKeyFile:
            privateKey = privateKeyFile.read()
        formattedPrivateKey = (
            "ED25519-V3:" +
            b2a_base64(privateKey[len("== ed25519v1-secret: type0 ==\x00\x00\x00"):]).strip()
        )
        public_port = int(public_port)

        if localPort is not None:
            localPort = int(localPort)

        if controlPort:
            try:
                ep = clientFromString(
                    reactor, "tcp:host=127.0.0.1:port=%d" % int(controlPort))
            except ValueError:
                ep = clientFromString(reactor, "unix:path=%s" % controlPort)
            construct = TCPHiddenServiceEndpoint.system_tor
        else:
            ep = None
            construct = TCPHiddenServiceEndpoint.global_tor

        return construct(
            reactor,
            ep,
            public_port,
            local_port=localPort,
            private_key=formattedPrivateKey,
            version=3,
        )

parser = TCPHiddenServiceEndpointParser()
