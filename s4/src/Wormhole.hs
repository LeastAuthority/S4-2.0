{-# LANGUAGE OverloadedStrings #-}
module Wormhole
  ( WormholeClient
  , WormholeCode
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  ) where

import Network.URI
  ( URI
  , uriToString
  )

import qualified Crypto.Spake2 as Spake2
import qualified MagicWormhole

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
  )

import Data.Aeson
  ( encode
  )

import Model
  ( Subscription
  )

-- A complete wormhole code including the nameplate and random words.
type WormholeCode = Text

type Error = String

type WormholeSend = IO ()

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
          let send =
                MagicWormhole.withEncryptedConnection peer spake2Password $ \conn -> do
                let offer = MagicWormhole.Message "blub"
                MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toStrict $ encode offer))
          return $ Right (code, send)

newPassword :: IO ByteString
newPassword = return "monkey-puppies"
