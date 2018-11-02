module S4.Server
  ( startServer
  ) where

import Network.Socket
  ( PortNumber
  )

import Servant
  ( Proxy
  )

import Network.URL
  ( importURL
  )

import Network.Wai.Handler.Warp
  ( Port
  , run
  )

import S4.Internal.Deployment
  ( Deployment(Deployment, wormholeDelivery)
  )

import S4.Internal.Wormhole
  ( WormholeServer(WormholeServer, wormholeServerRoot)
  )

import S4.Internal.API
  ( app
  )

-- Run a server for the API, indefinitely.
startServer
  :: Port      --  The TCP port number on which to listen for connections.
  -> IO ()
startServer portNumber =
  let
    Just rootURL = importURL "ws://wormhole.leastauthority.com:4000/v1"
    deployment = Deployment
      { wormholeDelivery = WormholeServer
        { wormholeServerRoot = rootURL
        }
      }
  in
    run portNumber $ app deployment
