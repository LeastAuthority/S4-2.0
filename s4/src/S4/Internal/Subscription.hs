module S4.Internal.Subscription
  ( Subscription(Subscription)
  , nextInvoice
  ) where

import S4.Internal.Invoice
  ( Invoice(Invoice)
  )

-- All of the configuration and state of a single S4 subscription.
data Subscription = Subscription

-- Construct the given subscription's next invoice.
nextInvoice :: Subscription -> Invoice
nextInvoice Subscription = Invoice
