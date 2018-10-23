{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model
  ( PlanID
  , Plan(Plan)
  , planID
  , planCurrency
  , planInterval
  , planPrice
  , FURL
  , Currency(ZEC)
  , SigningKey(SigningKey)
  , SubscriptionID
  , OnionKey
  , Storage(Storage, onionKey, frontendAddress, backendAddress)
  , PaymentStatus(PaymentStatus, paymentAddresses, creditAmount, dueDate, signingKey)
  , Subscription(Subscription, subscriptionID, subscriptionPlan, subscriptionStorage, paymentStatus)
  , secretKey
  , publicKey
  , newSubscription
  , nextDueDate
  , nextInvoiceURL
  ) where

import qualified Data.ByteString.Base64 as B64

import Network.URI
  ( URI(URI)
  , URIAuth(URIAuth)
  )

import Crypto.Saltine.Core.Sign
  ( Keypair
  , SecretKey
  , PublicKey
  , newKeypair
  )

import Crypto.Saltine.Core.Utils
  ( randomByteString
  )

import Data.Time
  ( UTCTime
  , NominalDiffTime
  , addUTCTime
  )

import Data.Scientific
  ( Scientific
  )

import Network.Socket
  ( HostName
  , PortNumber
  )

import Data.ByteString.Char8
  ( unpack
  )

import Data.Time
  ( UTCTime
  , NominalDiffTime
  )

import Crypto.Saltine.Core.Sign
  ( Keypair
  )

import Crypto.Saltine.Class
  ( encode
  )

-- A Zcash shielded address at which payment can be received
data ShieldedAddress = SproutAddress String
  deriving (Eq, Show)

-- An identifier for a subscription
type SubscriptionID = String

-- A Tor v3 Hidden Service key
type OnionKey = String

-- An identifier for a plan
type PlanID = String

newtype SigningKey = SigningKey Keypair deriving (Eq)

instance Show SigningKey where
  -- Let us differentiate between keys but without an overwhelming blob of
  -- random.
  show (SigningKey (_, publicKey)) = take 4 . unpack . encode $ publicKey

secretKey (SigningKey (secretKey, _)) = secretKey
publicKey (SigningKey (_, publicKey)) = publicKey

data Storage = Storage
  { onionKey           :: OnionKey     -- A Tor v3 Hidden Service key for a
                                       -- service pointing at this storage
                                       -- server.
  , frontendAddress    :: FURL         -- The onion-address fURL for this storage server.
  , backendAddress     :: Address      -- The physical backend address for
                                       -- this storage server.  Connections to
                                       -- frontendAddress are proxied to this
                                       -- location.
  } deriving (Eq, Show)

data Currency = ZEC deriving (Eq, Show)

data Plan = Plan
  { planID       :: PlanID
  , planInterval :: NominalDiffTime
  , planCurrency :: Currency
  , planPrice    :: Scientific
  } deriving (Eq, Show)

type FURL = String
type Address = (HostName, PortNumber)

-- An accounting relationship with a single user of the service.
data Subscription = Subscription
  { subscriptionID      :: SubscriptionID -- Unique identifier for this subscription
  , subscriptionPlan    :: Plan           -- The plan for this subscription
  , subscriptionStorage :: [Storage]      -- Information about Tahoe-LAFS storage servers for this subscription
  , paymentStatus       :: PaymentStatus  -- The details about money for this subscription
  } deriving (Eq, Show)

-- Current status with respect to payments and funding for a subscription.
data PaymentStatus = PaymentStatus
  { paymentAddresses :: [ShieldedAddress] -- Addresses at which payments for
                                          -- the subscription might arrive.
                                          -- The next payment is expected at
                                          -- the first address in this list.
                                          -- When addresses are rotated, new
                                          -- addresses are pushed on the
                                          -- front.
  , creditAmount     :: Scientific        -- Any overpayment on the last
                                          -- paid-in-full invoice which will
                                          -- be combined with any future
                                          -- payment to fully pay the next
                                          -- invoice.
  , dueDate          :: UTCTime           -- The time before which the next
                                          -- payment must be received to
                                          -- maintain the subscription in good
                                          -- standing.
  , signingKey       :: SigningKey        -- The key to use to sign invoices.
  } deriving (Eq, Show)


newSubscription :: IO UTCTime -> Plan -> IO Subscription
newSubscription getCurrentTime plan = do
  subscriptionID <- randomSubscriptionID
  onionKey <- randomOnionKey
  signingKey <- newSigningKey
  now <- getCurrentTime
  let paymentStatus = PaymentStatus [] 0 (nextDueDate now plan) signingKey
  return $ Subscription subscriptionID plan [Storage onionKey "blub" ("127.0.0.1", 12345)] paymentStatus

newSigningKey :: IO SigningKey
newSigningKey = newKeypair >>= return . SigningKey

randomSubscriptionID :: IO SubscriptionID
randomSubscriptionID = randomByteString 8 >>= return . B64.encode >>= return . unpack

-- XXX really generate a good tor key
randomOnionKey :: IO OnionKey
randomOnionKey = randomByteString 16 >>= return . unpack

nextDueDate :: UTCTime -> Plan -> UTCTime
nextDueDate from plan = addUTCTime (planInterval plan) from

nextInvoiceURL :: Subscription -> URI
nextInvoiceURL subscription = URI "http" (Just $ URIAuth "" "example.invalid" "") "/invoice.blub" "" ""
