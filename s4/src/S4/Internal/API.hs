-- Allow Servant API definitions
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}

-- Make string literals the right type
{-# LANGUAGE OverloadedStrings #-}

-- Implement the HTTP API for S4 subscription operations.

module S4.Internal.API
  ( CreateSubscriptionResult(WormholeInvitation)
  , app
  ) where

import GHC.Generics
  ( Generic
  )

import Control.Monad.IO.Class
  ( liftIO
  )

import Control.Monad.Extra
  ( liftMaybe
  )

import Control.Exception.Safe
  ( SomeException
  )

import qualified Data.Map as Map

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , genericToJSON
  , genericParseJSON
  , encode
  )

import Servant
  ( ServantErr(errBody)
  , Proxy(Proxy)
  , Application
  , Handler
  , JSON
  , ReqBody
  , PostCreated
  , Get
  , throwError
  , (:>)
  , (:<|>)((:<|>))
  )

import Servant.Server
  ( Server
  , serve
  , err403
  )

import S4.Internal.JSON
  -- This gives us the particular serialization style we prefer for JSON.
  ( jsonOptions
  )

import S4.Internal.Deployment
  ( Deployment(wormholeDelivery)
  )

import S4.Internal.Wormhole
  ( WormholeDelivery
  , WormholeCode(WormholeCode)
  )

import S4.Internal.Subscription
  ( Subscription(Subscription)
  , nextInvoice
  )

import S4.Internal.Invoice
  ( deliverInvoice
  )

import S4.Plan
  ( PlanID
  , Plan(Plan)
  )

-- Represent a request to create a new subscription.
data CreateSubscription =
  CreateSubscriptionForPlan
  { createForPlanID :: PlanID
  } deriving (Eq, Show, Generic)

instance ToJSON CreateSubscription where
  toJSON = genericToJSON jsonOptions

instance FromJSON CreateSubscription where
  parseJSON = genericParseJSON jsonOptions

-- Represent a response to a request to create a new subscription.
data CreateSubscriptionResult =
  -- The subscription was created and details about it can be retrieved using
  -- the contained code.
  WormholeInvitation
  { inviteCode :: WormholeCode
  }
  -- The plan specified is not known.
  | InvalidPlanID
  -- The subscription might have been created but there was a problem opening
  -- a wormhole to convey its details.
  | WormholeOpenFailed
  deriving (Eq, Show, Generic)

instance ToJSON CreateSubscriptionResult where
  toJSON = genericToJSON jsonOptions

instance FromJSON CreateSubscriptionResult where
  parseJSON = genericParseJSON jsonOptions

-- Define the API using Servant.
type API =
  -- List plans that exist and can be associated with a new subscription.
  "v1" :> "plans" :> Get '[JSON] [Plan]

  -- Attempt to create a new subscription.
  :<|> "v1" :> "subscriptions" :> ReqBody '[JSON] CreateSubscription :> PostCreated '[JSON] CreateSubscriptionResult


-- Attempt to create a new S4 subscription.
createSubscription
  :: (WormholeDelivery w)
  => w                                  -- A simplified interface to invoice delivery via Magic Wormhole.
  -> CreateSubscription                 -- Parameters relating to the new subscription.
  -> Handler CreateSubscriptionResult   -- Give back information describing the result of the attempt.
createSubscription wormhole (CreateSubscriptionForPlan planID) =
  case Map.lookup planID plans of
    Nothing ->
      -- The requested plan is not one we know about.
      throwError invalidPlanErr
      where
        invalidPlanErr :: ServantErr
        invalidPlanErr = err403 { errBody = encode InvalidPlanID }
    otherwise -> do
      -- TODO Actually persist the subscription and provision the resources it requires.
      let subscription = Subscription
      let invoice = nextInvoice subscription
      wormholeCode <- deliverInvoice wormhole invoice
      WormholeInvitation <$> wormholeCode

-- A bit of boilerplate required by Servant to glue things together.
api :: Proxy API
api = Proxy

-- Another bit of Servant boilerplate.
app :: WormholeDelivery w => Deployment w -> Application
app deployment = serve api (server deployment)

-- Collect the pieces of the implementation of the API into a whole.
server :: WormholeDelivery w => Deployment w -> Server API
server deployment = listPlans
  :<|> createSubscription (wormholeDelivery deployment)
  where
    listPlans :: Handler [Plan]
    listPlans = return . Map.elems $ plans

-- XXX This should be a parameter instead.
plans :: Map.Map PlanID Plan
plans = Map.fromList [("abcd", Plan "abcd")]
