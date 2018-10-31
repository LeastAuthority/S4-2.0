module S4.Internal.Deployment
  ( Deployment(Deployment, wormholeDelivery)
  ) where

import S4.Internal.Wormhole
  ( WormholeDelivery
  )

-- All of the configuration and state of an S4 deployment.
data Deployment w = Deployment
  { wormholeDelivery :: w
  }
