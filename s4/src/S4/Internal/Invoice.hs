module S4.Internal.Invoice
  ( Invoice(Invoice)
  , deliverInvoice
  , toURI
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Exception.Safe
  ( MonadThrow
  , throwM
  )

import Control.Concurrent.Async
  ( Async
  , async
  )

import Data.Text
  ( pack
  )

import Network.URI
  ( URI(..)
  , uriToString
  )

import S4.Internal.Wormhole
  ( WormholeDelivery(wormholeCodeGenerator, sendThroughWormhole)
  , WormholeCode
  )

-- All of the details of a single payment that is required to maintain a
-- subscription in good standing.
data Invoice = Invoice

toURI :: Invoice -> URI
toURI invoice = URI
  { uriScheme = ""
  , uriAuthority = Nothing
  , uriPath = ""
  , uriQuery = ""
  , uriFragment = ""
  }

deliverInvoice
  :: (MonadIO a, MonadThrow b, WormholeDelivery w)
  => w
  -> Invoice
  -> a (b WormholeCode)
deliverInvoice wormhole invoice = liftIO $ do
  wormholeCode <- wormholeCodeGenerator wormhole
  async $ do
      -- TODO Retry on errors where it makes sense - eg errors connecting to
      -- the Magic Wormhole server.  Propagate other errors to the
      -- subscription system, somehow, so the subscription associated with the
      -- invoice is marked as garbage (if this first invoice is not delivered
      -- then the legitimate owner of this subscription can never possibly use
      -- it; they should just retry).
    let uri = toURI invoice
    let uriString = (uriToString id uri) ""
    let uriText = pack uriString
    delivery <- sendThroughWormhole wormhole uriText wormholeCode
    case delivery of
      Left err -> throwM err
      Right () -> return ()

  return $ return wormholeCode
