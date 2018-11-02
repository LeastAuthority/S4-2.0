{-# LANGUAGE OverloadedStrings #-}

module S4.Internal.Invoice
  ( Invoice
  , invoice
  , deliverInvoice
  , toText
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

import qualified Data.ByteString.Base64.URL as B64U

import Data.ByteString.UTF8
  ( toString
  )

import Data.Text
  ( Text
  , pack
  , unpack
  )

import Network.URL
  ( URL(URL, url_type, url_path, url_params)
  , URLType(PathRelative)
  , Host(Host)
  , exportURL
  )

import S4.Internal.Wormhole
  ( WormholeDelivery(wormholeCodeGenerator, sendThroughWormhole)
  , WormholeCode
  )

data InvoiceKey =
  Currency
  | Version
  | Amount
  | DueDate
  | ExtensionDate
  | ServiceLabel
  | NextInvoiceURL
  | Credit
  | PublicKey
  | Signature
  | Message
  deriving (Eq, Show)

invoiceKey :: InvoiceKey -> String
invoiceKey Version = "v"
invoiceKey Currency = "c"
invoiceKey Amount = "a"
invoiceKey DueDate = "d"
invoiceKey ExtensionDate = "e"
invoiceKey ServiceLabel = "l"
invoiceKey NextInvoiceURL = "u"
invoiceKey Credit = "r"
invoiceKey PublicKey = "p"
invoiceKey Message = "m"
invoiceKey Signature = "s"

-- All of the details of a single payment that is required to maintain a
-- subscription in good standing.
data Invoice = Invoice
  { label :: Text
  }

-- Create an S4 2.0 invoice.
invoice :: Invoice
invoice = Invoice
  { label = "Least Authority S4 2.0"
  }

-- Serialize an invoice to the URI-based text format.
toText :: Invoice -> Text
toText = pack . exportURL . toURL

-- Convert an invoice to an intermediate URI representation.
toURL :: Invoice -> URL
toURL invoice = URL
  { url_type = PathRelative
  , url_path = ""
  , url_params =
    [ (invoiceKey ServiceLabel, toString . B64U.encode $ "Least Authority S4 2.0")
    ]
  }

-- Send an invoice through a new Magic Wormhole.
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
    delivery <- sendThroughWormhole wormhole (toText invoice) wormholeCode
    case delivery of
      Left err -> throwM err
      Right () -> return ()

  return $ return wormholeCode
