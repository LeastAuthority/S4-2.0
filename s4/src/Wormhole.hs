{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Wormhole
  ( WormholeClient
  , WormholeCode
  , AbilitiesContainer(AbilitiesContainer)
  , Abilities(Abilities)
  , ClientV1(ClientV1)
  , ServerV1(ServerV1)
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
  , omitNothingFields
  , camelTo2
  , genericToJSON
  , genericParseJSON
  )

import Model
  ( Subscription
  )

import Tahoe
  ( Configuration
  , fromSubscription
  )

-- A complete wormhole code including the nameplate and random words.
type WormholeCode = Text

data Error =
  UnparseableWebSocketEndpoint
  deriving (Eq, Show)

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
        return $ Left UnparseableWebSocketEndpoint
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

        sendAbilities conn = sendJSON (AbilitiesContainer (Abilities Nothing (Just ServerV1))) conn

        receiveAbilities :: MagicWormhole.EncryptedConnection -> IO (Subscription -> Either Text Configuration)
        receiveAbilities conn = do
          (MagicWormhole.PlainText introText) <- atomically $ MagicWormhole.receiveMessage conn
          let introMessage = decode $ fromStrict introText
          case introMessage of
            Just (AbilitiesContainer (Abilities (Just ClientV1) Nothing)) ->
              return encodeConfigV1
            otherwise ->
              return unsupportedConfig

        sendJSON :: ToJSON a => a -> MagicWormhole.EncryptedConnection -> IO ()
        sendJSON obj conn = do
          let msg = MagicWormhole.Message $ decodeUtf8 $ toStrict $ encode obj
          MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toStrict $ encode msg))

encodeConfigV1 :: Subscription -> Either Text Configuration
encodeConfigV1 = Right . fromSubscription

unsupportedConfig :: ToJSON o => a -> Either Text o
unsupportedConfig anything = Left "blub blub"

newPassword :: IO ByteString
newPassword = return "monkey-puppies"

-- Aeson encoding options that turns camelCase into hyphenated-words.
jsonOptions = defaultOptions
  { fieldLabelModifier =
      let
        fixUnderscores '_' = '-'
        fixUnderscores c = c
      in
        (camelTo2 '-') . (map fixUnderscores)
  , omitNothingFields = True
  }

data ClientV1 = ClientV1 deriving (Eq, Show)

instance ToJSON ClientV1 where
  toJSON anything = object []

instance FromJSON ClientV1 where
  parseJSON anything = return ClientV1

data ServerV1 = ServerV1 deriving (Eq, Show)

instance ToJSON ServerV1 where
  toJSON anything = object []

instance FromJSON ServerV1 where
  parseJSON anything = return ServerV1

data AbilitiesContainer = AbilitiesContainer
  { abilities :: Abilities
  } deriving (Eq, Show, Generic)

instance ToJSON AbilitiesContainer where
  toJSON = genericToJSON jsonOptions

instance FromJSON AbilitiesContainer where
  parseJSON = genericParseJSON jsonOptions

data Abilities = Abilities
  { client_v1 :: Maybe ClientV1
  , server_v1 :: Maybe ServerV1
  } deriving (Eq, Show, Generic)

instance ToJSON Abilities where
  toJSON = genericToJSON jsonOptions

instance FromJSON Abilities where
  parseJSON = genericParseJSON jsonOptions
