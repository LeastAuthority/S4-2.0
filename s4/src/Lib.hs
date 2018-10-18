{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , app
  , CreateSubscriptionResult(WormholeInvitation)
  , CreateSubscription(CreateSubscriptionForPlan)
  ) where

import Wormhole
  ( WormholeClient
  , WormholeCode
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  )

import qualified Data.Map as Map
import GHC.Generics
  ( Generic
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8
  ( unpack
  )
import Data.Aeson hiding
  ( encode
  )
import Data.Aeson.TH
import Data.Time
  ( UTCTime
  , NominalDiffTime
  , addUTCTime
  )
import Data.Time.Clock
  ( getCurrentTime
  )
import Crypto.Saltine.Core.Utils
  ( randomByteString
  )
import Crypto.Saltine.Core.Hash
  ( hash
  )
import Crypto.Saltine.Core.Sign
  ( Keypair
  , SecretKey
  , PublicKey
  , newKeypair
  )
import Network.URI
  ( parseURI
  )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Model
  ( PlanID
  , SigningKey(SigningKey)
  , SubscriptionID
  , OnionKey
  , PaymentStatus(PaymentStatus)
  , Subscription(Subscription)
  )

-- Aeson encoding options that turns camelCase into hyphenated-words.
jsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '-'
  }

data CreateSubscription =
  CreateSubscriptionForPlan
  { createForPlanID :: PlanID
  } deriving (Eq, Show, Generic)

instance ToJSON CreateSubscription where
  toJSON = genericToJSON jsonOptions

instance FromJSON CreateSubscription where
  parseJSON = genericParseJSON jsonOptions

data CreateSubscriptionResult =
  WormholeInvitation
  { inviteCode :: WormholeCode
  }
  | InvalidPlanID
  | WormholeOpenFailed
  deriving (Eq, Show, Generic)

instance ToJSON CreateSubscriptionResult where
  toJSON = genericToJSON jsonOptions

type API = "v1" :> "subscriptions" :> ReqBody '[JSON] CreateSubscription :> PostCreated '[JSON] CreateSubscriptionResult

startApp :: IO ()
startApp =
  case run 8080 <$> (app <$> NetworkWormholeClient <$> parseURI "ws://wormhole.leastauthority.com:4000/v1") of
    Nothing ->
      putStrLn "Failed to start application"
    Just it ->
      it

app :: WormholeClient c => c -> Application
app wormholeClient = serve api (server wormholeClient)

api :: Proxy API
api = Proxy

server :: WormholeClient c => c -> Server API
server wormholeClient = createSubscription wormholeClient

data Plan = Plan
  { planID       :: PlanID
  , planInterval :: NominalDiffTime
  } deriving (Eq, Show)

plans :: Map.Map PlanID Plan
plans = Map.fromList [("abcd", Plan "abcd" 3600)]

createSubscription :: WormholeClient c => c -> CreateSubscription -> Handler CreateSubscriptionResult
createSubscription wormholeClient (CreateSubscriptionForPlan id) = liftIO $
  case Map.lookup id plans of
    Nothing ->
      return InvalidPlanID
    Just plan -> do
      subscription <- newSubscription plan
      openAttempt <- sendSubscription wormholeClient subscription
      case openAttempt of
        Left err ->
          return WormholeOpenFailed
        Right (wormholeCode, send) ->
          return $ WormholeInvitation wormholeCode

newSubscription :: Plan -> IO Subscription
newSubscription plan = do
  subscriptionID <- randomSubscriptionID
  onionKey <- randomOnionKey
  signingKey <- newSigningKey
  now <- getCurrentTime
  let paymentStatus = PaymentStatus [] 0 (nextDueDate now plan) signingKey
  return $ Subscription subscriptionID (planID plan) onionKey paymentStatus

newSigningKey :: IO SigningKey
newSigningKey = newKeypair >>= return . SigningKey

randomSubscriptionID :: IO SubscriptionID
randomSubscriptionID = randomByteString 8 >>= return . B64.encode >>= return . unpack

-- XXX really generate a good tor key
randomOnionKey :: IO OnionKey
randomOnionKey = randomByteString 16 >>= return . unpack

nextDueDate :: UTCTime -> Plan -> UTCTime
nextDueDate from plan = addUTCTime (planInterval plan) from
