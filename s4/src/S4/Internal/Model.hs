module S4.Internal.Model
  ( Deployment(Deployment, wormholeCodeGenerator)
  , Subscription(Subscription)
  , Invoice
  , nextInvoice
  ) where

import S4.Internal.Wormhole
  ( WormholeCode
  )

-- All of the configuration and state of an S4 deployment.
data Deployment = Deployment
  { wormholeCodeGenerator :: IO WormholeCode
  }

-- All of the configuration and state of a single S4 subscription.
data Subscription = Subscription

-- All of the details of a single payment that is required to maintain a
-- subscription in good standing.
data Invoice = Invoice

-- Construct the given subscription's next invoice.
nextInvoice :: Subscription -> Invoice
nextInvoice anything = Invoice
