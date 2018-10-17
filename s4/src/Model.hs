{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model
  ( PlanID
  , SigningKey(SigningKey)
  , SubscriptionID
  , OnionKey
  , PaymentStatus(PaymentStatus)
  , Subscription(Subscription)
  ) where

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

-- One hundred millionth of one Zcash coin
newtype Zatoshi = Zatoshi Integer deriving (Eq, Show, Ord, Num)

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

-- An accounting relationship with a single user of the service.
data Subscription = Subscription
  { id                 :: SubscriptionID -- Unique identifier for this subscription
  , subscriptionPlanID :: PlanID         -- A reference to this subscription's plan
  , onionKey           :: OnionKey       -- A Tor v3 Hidden Service key for this subscription
  , paymentStatus      :: PaymentStatus  -- The details about money for this subscription
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
  , creditAmount     :: Zatoshi           -- Any overpayment on the last
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
