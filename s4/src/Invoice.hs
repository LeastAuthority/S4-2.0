{-# LANGUAGE OverloadedStrings #-}

module Invoice
  ( encode
  ) where

import Data.ByteString.UTF8
  ( toString
  , fromString
  )
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Lazy
  ( toStrict
  )

import Data.URLEncoded
  ( importList
  , export
  , (%&)
  , (%=)
  )

import Data.Time.Format
  ( formatTime
  , defaultTimeLocale
  )

import qualified Data.Aeson as Aeson

import Crypto.Saltine.Core.Sign
  ( sign
  )

import qualified Crypto.Saltine.Class as Saltine

import Model
  ( Subscription(Subscription)
  , subscriptionPlan
  , planCurrency
  , planPrice
  , dueDate
  , paymentStatus
  , nextDueDate
  , signingKey
  , secretKey
  , publicKey
  , creditAmount
  )

import Tahoe
  ( fromSubscription
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

encode :: Subscription -> String
encode subscription =
  let plan = subscriptionPlan subscription
      status = paymentStatus subscription
      pubKey = toString . B64U.encode . Saltine.encode . publicKey . signingKey
      tahoeConfig = toString . toStrict . Aeson.encode . fromSubscription
      iso8601Format = formatTime defaultTimeLocale "%FT%T"
      unsigned =
        importList
        [ (invoiceKey Version, "1")
        , (invoiceKey Currency, show . planCurrency $ plan)
        , (invoiceKey Amount, show . planPrice $ plan)
        , (invoiceKey DueDate, iso8601Format . dueDate $ status)
        , (invoiceKey ExtensionDate, iso8601Format $ nextDueDate (dueDate status) plan)
        , (invoiceKey ServiceLabel, toString . B64U.encode $ "Least Authority S4 / P4")
        , (invoiceKey NextInvoiceURL, toString . B64U.encode $ "blub")
        , (invoiceKey Credit, show . creditAmount $ status)
        , (invoiceKey PublicKey, pubKey status)
        , (invoiceKey Message, tahoeConfig subscription)
        ]
      signature = sign (secretKey . signingKey . paymentStatus $ subscription) (fromString . export $ unsigned)
      signed = unsigned %& importList [(invoiceKey Signature, toString signature)]
  in
    export signed

type Currency = Scientific
type Signature = ByteString

data Invoice =
  InvoiceV1
  { invoiceAmount :: Currency
  , invoiceDueDate :: UTCTime
  , invoiceExtensionDate :: UTCTime
  , invoiceLabel :: Text
  , invoiceNextURI ::  URI
  , invoiceCredit :: Currency

  -- This is the public part of the keypair which will sign the invoice that
  -- follows this one.  This allows for key rotation on the service-provider
  -- side should keys be compromised (or as a preventative measure to avoid
  -- this).  Only the public part of this keypair is available here because
  -- the private parts are restricted to storage and processing in a more
  -- easily secured context (where fewer software components are involved, for
  -- example).
  , invoiceNextKey :: PublicKey

  -- This is the signature of the serialized form of this very invoice.  It is
  -- computed elsewhere and shipped here because we lack the secret key
  -- necessary to compute it (by design).
  , invoiceSignature :: Signature
  }

fromSubscription :: UTCTime -> Subscription -> Invoice
fromSubscription now subscription =
  let plan = subscriptionPlan subscription
  in
    InvoiceV1
    { invoiceAmount = planPrice plan
    , invoiceDueDate = nextDueDate now plan
    , invoiceExtensionDate = nextExtensionDate now plan
