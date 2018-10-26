{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Wormhole
  ( Error(UnparseableWebSocketEndpoint)
  , WormholeClient
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

import Control.Exception.Safe
  ( finally
  )

import Control.Monad.STM
  ( atomically
  )

import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
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
  ( Subscription(subscriptionID)
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
  sendSubscription :: c -> WormholeCode -> Subscription -> IO (Either Error ())

data NetworkWormholeClient =
  NetworkWormholeClient URI
  deriving (Eq, Show)

instance WormholeClient NetworkWormholeClient where
  sendSubscription (NetworkWormholeClient root) code subscription = do
    putStrLn $ "Sending " ++ (subscriptionID subscription)
    let appID = MagicWormhole.AppID "tahoe-lafs.org/invite"
    let endpoint = uriToString id root ""
    case MagicWormhole.parseWebSocketEndpoint endpoint of
      Nothing ->
        return $ Left UnparseableWebSocketEndpoint
      Just wsEndpoint -> do
        side <- MagicWormhole.generateSide
        putStrLn $ show side
        MagicWormhole.runClient wsEndpoint appID side sendInvite
      where
        sendInvite session = do
          let [nameplate, first, second] = Text.split (== '-') code
          let password = first <> "-" <> second
          let spake2Password = Spake2.makePassword $ encodeUtf8 code

          putStrLn "Claiming nameplate"
          mailbox <- MagicWormhole.claim session (MagicWormhole.Nameplate nameplate)
          putStrLn "Opening mailbox"
          peer <- MagicWormhole.open session mailbox
          putStrLn "Setting up encrypted connection handler"
          let send = MagicWormhole.withEncryptedConnection peer spake2Password $ sendSubscr' subscription
          putStrLn "Setting up final close"
          let close = MagicWormhole.close session (Just mailbox) Nothing
          putStrLn "Executing"
          finally send close
          return $ Right ()

        sendSubscr' subscription conn = do
          putStrLn "Sending abilities"
          sendAbilities conn
          putStrLn "Receiving abilities"
          encodeConfig <- receiveAbilities conn
          putStrLn "Received abilities"
          let encoded = encodeConfig subscription
          case encoded of
            Left err -> do
              -- XXX This error does not seem to propagate correctly to the caller.
              putStrLn $ "Error encoding subscription " <> (show err)
              return $ Left err
            Right msg -> do
              putStrLn "Sending configuration"
              sendJSON msg conn
              return $ Right ()

        sendAbilities conn = sendJSON (AbilitiesContainer (Abilities Nothing (Just ServerV1))) conn

        receiveAbilities :: MagicWormhole.EncryptedConnection -> IO (Subscription -> Either Text Configuration)
        receiveAbilities conn = do
          (MagicWormhole.PlainText introText) <- atomically $ MagicWormhole.receiveMessage conn
          putStrLn "Received raw abilities message"
          TextIO.putStrLn $ (decodeUtf8 introText)
          let introMessage = decode $ fromStrict introText
          case introMessage of
            Nothing -> do
              putStrLn $ "Instead of Message, received something weird: " <> show introMessage
              return unsupportedConfig

            Just (MagicWormhole.Message message) ->
              let
                abilitiesMessage = decode $ fromStrict (encodeUtf8 message)
              in
                case abilitiesMessage of
                  Just (AbilitiesContainer (Abilities (Just ClientV1) Nothing)) -> do
                    putStrLn "Received ClientV1 abilities"
                    return encodeConfigV1
                  otherwise -> do
                    putStrLn $ "Instead of abilities, received something weird: " <> show message
                    return unsupportedConfig

        sendJSON :: ToJSON a => a -> MagicWormhole.EncryptedConnection -> IO ()
        sendJSON obj conn = do
          let msg = MagicWormhole.Message $ decodeUtf8 $ toStrict $ encode obj
          MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toStrict $ encode msg))

encodeConfigV1 :: Subscription -> Either Text Configuration
encodeConfigV1 = Right . fromSubscription

unsupportedConfig :: ToJSON o => a -> Either Text o
unsupportedConfig anything = Left "blub blub"

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
