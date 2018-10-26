{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
  ( startApp
  , app
  , CreateSubscriptionResult(WormholeInvitation, InvalidPlanID)
  , CreateSubscription(CreateSubscriptionForPlan)
  ) where

import System.Entropy
  ( getEntropy
  )

import Prelude hiding
  ( head
  )

import Text.Printf
  ( printf
  )

import Data.Char
  ( ord
  )

import Data.Text.PgpWordlist
  ( toText
  )

import qualified Wormhole

import Wormhole
  ( WormholeClient
  , WormholeCode
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  )

import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Data.Text
  ( Text
  )

import Data.ByteString.Lazy
  ( toStrict
  , fromStrict
  )

import Data.ByteString
  ( head
  )

import Data.Time.Clock
  ( getCurrentTime
  )

import Control.Exception.Safe
  ( SomeException
  , catchAny
  )
import Control.Concurrent.Async
  ( Async
  , async
  , waitCatch
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
      :<|> "v1" :> "plans" :> Get '[JSON] [Plan]

startApp :: IO ()
startApp =
  let
    -- TODO: Command line option for listen address - including Tor support, probably.
    serverPort = 8080
    -- wormholeMailboxURI = "ws://wormhole.leastauthority.com:4000/v1"
    wormholeMailboxURI = "ws://localhost:4000/v1"
  in
    case run serverPort <$> (app <$> NetworkWormholeClient <$> parseURI wormholeMailboxURI) of
      Nothing ->
        -- TODO: Reflect this in the exit status and try to report more information too.
        putStrLn "Failed to start application"
      Just it -> do
        putStrLn "Starting"
        it

app :: WormholeClient c => c -> Application
app wormholeClient = serve api (server wormholeClient)

api :: Proxy API
api = Proxy

server :: WormholeClient c => c -> Server API
server wormholeClient = createSubscription wormholeClient
                   :<|> (return . Map.elems $ plans)

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
      let
        handler :: Either Wormhole.Error () -> IO (Either Wormhole.Error ())
        handler (Left e) = do
          putStrLn $ "Sending configuration failed" <> show e
          return $ Right ()
        handler (Right ()) = do
          putStrLn "Sending configuration succeeded"
          return $ Right ()

      subscription <- newSubscription getCurrentTime plan
      wormholeCode <- newWormholeCode
      TextIO.putStrLn $ "Got wormhole code " <> wormholeCode
      TextIO.putStrLn "Offering the configuration"
      sending <- async $ sendSubscription wormholeClient wormholeCode subscription >>= handler
      return $ WormholeInvitation wormholeCode

newWormholeCode :: IO WormholeCode
newWormholeCode = do
  nameplate <- newNameplate
  password <- newPassword
  return $ nameplate <> "-" <> password

newPassword :: IO Text
newPassword =
  let
    fixSpace ' ' = '-'
    fixSpace c = c
    hyphenate = Text.map fixSpace
  in
    getEntropy 2 >>= return . hyphenate . toText . fromStrict

newNameplate :: IO Text
newNameplate = do
  entropy <- getEntropy 1
  let ch = head entropy
  let i = toInteger ch
  let n = i + 100
  let s = show n
  return $ Text.pack s
