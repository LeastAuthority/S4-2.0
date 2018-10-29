-- Allow Servant API definitions
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}

-- Make string literals the right type
{-# LANGUAGE OverloadedStrings #-}

module S4.Internal.API
  ( CreateSubscriptionResult(WormholeInvitation)
  , app
  ) where

import GHC.Generics
  ( Generic
  )

import Control.Monad.Extra
  ( liftMaybe
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

import S4.Internal.Wormhole
  ( WormholeCode(WormholeCode)
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


createSubscription :: CreateSubscription -> Handler CreateSubscriptionResult
createSubscription (CreateSubscriptionForPlan planID) = do
  let wormholeCode = WormholeCode 101 ["monoidal", "endofunctors"]
  case Map.lookup planID plans of
    Nothing ->
      throwError invalidPlanErr
      where
        invalidPlanErr :: ServantErr
        invalidPlanErr = err403 { errBody = encode InvalidPlanID }
    otherwise ->
      return $ WormholeInvitation wormholeCode

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

-- Collect the pieces of the implementation of the API into a whole.
server :: Server API
server = listPlans
  :<|> createSubscription
  where
    listPlans :: Handler [Plan]
    listPlans = return . Map.elems $ plans

-- XXX This should be a parameter instead.
plans :: Map.Map PlanID Plan
plans = Map.fromList [("abcd", Plan "abcd")]
