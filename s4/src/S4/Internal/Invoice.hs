module S4.Internal.Invoice
  ( deliverInvoice
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Exception.Safe
  ( MonadThrow
  , throwString
  )

import S4.Internal.Model
  ( Invoice
  )

-- Send an invoice along a Magic Wormhole.
-- TODO Really implement this
deliverInvoice
  :: (MonadIO a, MonadThrow b)
  => Invoice                    -- The invoice to deliver.
  -> wormholeCode               -- The wormhole code for the wormhole to use for delivery.
  -> a (b ())
deliverInvoice invoice wormholeCode = do
  return $ throwString ""
