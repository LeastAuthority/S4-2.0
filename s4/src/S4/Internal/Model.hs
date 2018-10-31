module S4.Internal.Model
  ( Deployment(Deployment, wormholeCodeGenerator)
  ) where

import S4.Internal.Wormhole
  ( WormholeCode
  )

-- A complete description of the configuration and state of an S4 deployment.
data Deployment = Deployment
  { wormholeCodeGenerator :: IO WormholeCode
  }
