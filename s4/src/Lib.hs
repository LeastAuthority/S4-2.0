{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , app
  , CreateSubscriptionResult(WormholeInvitation, InvalidPlanID)
  , CreateSubscription(CreateSubscriptionForPlan)
  ) where

import Wormhole
  ( WormholeClient
  , WormholeCode
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  )

import Data.Time.Clock
  ( getCurrentTime
  )

import Control.Concurrent
  ( forkIO
  )
import Control.Monad.Except
  ( throwError
  )
import qualified Data.Map as Map
import GHC.Generics
  ( Generic
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8
  ( unpack
  )
import Data.Aeson
import Data.Aeson.TH
import Crypto.Saltine.Core.Hash
  ( hash
  )
import Network.URI
  ( parseURI
  )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
  ( err403
  )

import Model
  ( PlanID
  , Plan(Plan)
  , planID
  , planInterval
  , SigningKey(SigningKey)
  , SubscriptionID
  , OnionKey
  , Storage(Storage)
  , Currency(ZEC)
  , PaymentStatus(PaymentStatus)
  , Subscription(Subscription)
  , newSubscription
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
  -- TODO: Command line option for listen address - including Tor support, probably.
  case run 8080 <$> (app <$> NetworkWormholeClient <$> parseURI "ws://wormhole.leastauthority.com:4000/v1") of
    Nothing ->
      -- TODO: Reflect this in the exit status and try to report more information too.
      putStrLn "Failed to start application"
    Just it ->
      it

app :: WormholeClient c => c -> Application
app wormholeClient = serve api (server wormholeClient)

api :: Proxy API
api = Proxy

server :: WormholeClient c => c -> Server API
server wormholeClient = createSubscription wormholeClient

plans :: Map.Map PlanID Plan
plans = Map.fromList [("abcd", Plan "abcd" 3600 ZEC (read "0.2"))]

createSubscription :: WormholeClient c => c -> CreateSubscription -> Handler CreateSubscriptionResult
createSubscription wormholeClient (CreateSubscriptionForPlan id) =
  case Map.lookup id plans of
    Nothing ->
      throwError invalidPlanErr
      where
        invalidPlanErr :: ServantErr
        invalidPlanErr = err403 { errBody = encode InvalidPlanID }
    Just plan -> liftIO $ do
      subscription <- newSubscription getCurrentTime plan
      openAttempt <- sendSubscription wormholeClient subscription
      case openAttempt of
        Left err ->
          return WormholeOpenFailed
        Right (wormholeCode, send) -> do
          putStrLn "Got wormhole code"
          forkIO $ send >>= \x -> do
            return ()
          return $ WormholeInvitation wormholeCode
