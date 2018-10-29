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

import S4.Internal.API
  ( app
  )

-- Run a server for the API on the given port number.
startServer :: Port -> IO ()
startServer portNumber = run portNumber app
