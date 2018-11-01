module S4.Internal.Deployment
  ( Deployment(Deployment, wormholeDelivery)
  ) where

import S4.Internal.Wormhole
  ( WormholeDelivery
  )

-- All of the configuration and state of an S4 deployment.
-- TODO Decide if a GADT is better suited (than this unconstrained polymorphism) for this type
data Deployment w = Deployment
  { wormholeDelivery :: w
  }
