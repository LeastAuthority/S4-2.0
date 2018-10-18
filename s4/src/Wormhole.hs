{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Wormhole
  ( WormholeClient
  , WormholeCode
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  ) where

import GHC.Generics
  ( Generic
  )

import Network.URI
  ( URI
  , uriToString
  )

import qualified Crypto.Spake2 as Spake2
import qualified MagicWormhole

import Control.Monad.STM
  ( atomically
  )

import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )

import Data.Text
  ( Text
  )

import Data.ByteString
  ( ByteString
  )

import Data.ByteString.Lazy
  ( toStrict
  , fromStrict
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , Object
  , (.=)
  , (.:)
  , object
  , encode
  , decode
  , withObject
  , defaultOptions
  , fieldLabelModifier
  , camelTo2
  , genericToJSON
  , genericParseJSON
  )

import Model
  ( Subscription
  )

-- A complete wormhole code including the nameplate and random words.
type WormholeCode = Text

type Error = String

type WormholeSend = IO (Either Text ())

class WormholeClient c where
  sendSubscription :: c -> Subscription -> IO (Either Error (WormholeCode, WormholeSend))

data NetworkWormholeClient =
  NetworkWormholeClient URI
  deriving (Eq, Show)

instance WormholeClient NetworkWormholeClient where
  sendSubscription (NetworkWormholeClient root) subscription = do
    let appID = MagicWormhole.AppID "tahoe-lafs.org/invite"
    let endpoint = uriToString id root ""
    case MagicWormhole.parseWebSocketEndpoint endpoint of
      Nothing ->
        return $ Left "oops"
      Just wsEndpoint -> do
        side <- MagicWormhole.generateSide
        MagicWormhole.runClient wsEndpoint appID side sendInvite
      where
        sendInvite session = do
          nameplate <- MagicWormhole.allocate session
          mailbox <- MagicWormhole.claim session nameplate
          peer <- MagicWormhole.open session mailbox  -- XXX: We should run `close` in the case of exceptions?
          password <- newPassword
          let (MagicWormhole.Nameplate n) = nameplate
          let code = n <> "-" <> (decodeUtf8 password)
          let spake2Password = Spake2.makePassword $ encodeUtf8 code
          let send = MagicWormhole.withEncryptedConnection peer spake2Password $ sendSubscr' subscription
          return $ Right (code, send)

        sendSubscr' subscription conn = do
          sendAbilities conn
          encodeConfig <- receiveAbilities conn
          let encoded = encodeConfig subscription
          case encoded of
            Left err -> return $ Left err
            Right msg -> do
              sendJSON msg conn
              return $ Right ()

        sendAbilities conn = sendJSON (Abilities ClientV1) conn

        sendConfig config conn = sendJSON conn

        receiveAbilities :: MagicWormhole.EncryptedConnection -> IO (Subscription -> Either Text Object)
        receiveAbilities conn = do
          (MagicWormhole.PlainText introText) <- atomically $ MagicWormhole.receiveMessage conn
          let introMessage = decode $ fromStrict introText
          case introMessage of
            Just (Abilities ClientV1) ->
              return encodeConfigV1
            otherwise ->
              return unsupportedConfig

        sendJSON obj conn = do
          let msg = MagicWormhole.Message $ decodeUtf8 $ toStrict $ encode obj
          MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toStrict $ encode msg))


encodeConfigV1 subscription = Right mempty

unsupportedConfig anything = Left "blub blub"

newPassword :: IO ByteString
newPassword = return "monkey-puppies"

-- Aeson encoding options that turns camelCase into hyphenated-words.
jsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '-'
  }

data ClientV1 = ClientV1 deriving (Eq, Show)

instance ToJSON ClientV1 where
  toJSON anything = object []

instance FromJSON ClientV1 where
  parseJSON anything = return ClientV1

data Abilities = Abilities
  { clientV1 :: ClientV1
  } deriving (Eq, Show, Generic)

instance ToJSON Abilities where
  toJSON = genericToJSON jsonOptions

instance FromJSON Abilities where
  parseJSON = genericParseJSON jsonOptions

data Configuration =
  ConfigurationV1
  { introducerfURL :: Text
  } deriving (Eq, Show)
