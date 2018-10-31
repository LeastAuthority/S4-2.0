module S4.Internal.Invoice
  ( Invoice(Invoice)
  , deliverInvoice
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Exception.Safe
  ( MonadThrow
  , throwString
  )

-- All of the details of a single payment that is required to maintain a
-- subscription in good standing.
data Invoice = Invoice

-- Send an invoice along a Magic Wormhole.
-- TODO Really implement this
deliverInvoice
  :: (MonadIO a, MonadThrow b)
  => Invoice                    -- The invoice to deliver.
  -> wormholeCode               -- The wormhole code for the wormhole to use for delivery.
  -> a (b ())
deliverInvoice invoice wormholeCode = do
  return $ throwString ""
