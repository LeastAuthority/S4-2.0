module S4.Server
  ( startServer
  ) where

import Network.Socket
  ( PortNumber
  )

import Servant
  ( Proxy
  )

import Network.Wai.Handler.Warp
  ( Port
  , run
  )

import S4.Internal.Model
  ( Deployment(Deployment, wormholeCodeGenerator)
  )

import S4.Internal.Wormhole
  ( newWormholeCode
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
    deployment = Deployment { wormholeCodeGenerator = newWormholeCode }
  in
    run portNumber $ app deployment
