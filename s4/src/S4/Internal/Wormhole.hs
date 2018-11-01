-- Simplify exception definition
{-# LANGUAGE DeriveAnyClass #-}

-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}

-- Make string literals the right type
{-# LANGUAGE OverloadedStrings #-}

module S4.Internal.Wormhole
  ( WormholeCode(WormholeCode)
  , WormholeDelivery(wormholeCodeGenerator, sendThroughWormhole)
  , WormholeServer(WormholeServer, wormholeServerRoot)
  , newWormholeCode
  ) where

import Prelude hiding
  ( head
  )

import Control.Monad
  ( fail
  )

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Exception.Safe
  ( Exception
  , MonadThrow
  , throwM
  , finally
  )

import Data.Map
  ( Map
  )

import qualified Data.Text.IO as TextIO
import Data.Text
  ( Text
  , intercalate
  , pack
  , unpack
  , split
  )
import qualified Data.Text as Text

import Data.ByteString
  ( head
  )

import Data.ByteString.Lazy
  ( fromStrict
  , toStrict
  )

import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , Value(String)
  , withText
  , encode
  )

import Data.Text.PgpWordlist
  ( toText
  )

import System.Entropy
  ( getEntropy
  )

import Network.URL
  ( URL
  , exportURL
  )

import qualified Crypto.Spake2 as Spake2
import qualified MagicWormhole

-- A structured representation of a Magic Wormhole code.
data WormholeCode =
-- The default number of passwords is two though Magic Wormhole allows this to
-- be configured on a per-wormhole basis.
  WormholeCode
  { nameplate :: Nameplate
  , password :: [Password]
  } deriving (Eq)

instance Show WormholeCode where
  show (WormholeCode nameplate password) = show nameplate <> "-" <> (unpack (intercalate "-" password))

-- Create the canonical SPAKE2 password  for a given Magic Wormhole code.
makeSpake2Password :: WormholeCode -> Spake2.Password
makeSpake2Password = Spake2.makePassword . encodeUtf8 . pack . show

-- Components of the code which is serialized like <nameplate>-<password>-<password>.
type Nameplate = Integer
type Password = Text

-- Serialize a Magic Wormhole code in the usual way for JSON.
instance ToJSON WormholeCode where
  toJSON (WormholeCode nameplate passwords) =
    String $ pack (show nameplate) <> "-" <> (intercalate "-" passwords)

-- Implement the inverse of ToJSON.
-- TODO Better error handling and input checking.
-- TODO Only allow passwords from the PGP wordlist.
instance FromJSON WormholeCode where
  parseJSON = withText "WormholeCode" $ \s ->
    let
      parse (nameplate:first:second:[]) =
        return $ WormholeCode (read (unpack nameplate) :: Nameplate) [first, second]
      parse otherwise =
        fail $ "WormholeCode not parsed from " <> unpack s

      parts :: [Text]
      parts = split ((==) '-') s
    in
        parse parts

-- Generate a new, random wormhole code.  There is no guarantee of nameplate
-- non-collision.
newWormholeCode :: IO WormholeCode
newWormholeCode = do
  nameplate <- newNameplate
  password <- newPassword 2
  return $ WormholeCode nameplate password

-- Generate a new, random password for a wormhole code.
newPassword :: Int -> IO [Text]
newPassword words =
  getEntropy words >>= return . split ((==) ' ') . toText . fromStrict

-- Generate a new, random nameplate for a wormhole code.  There is no
-- guarantee of non-collision.
newNameplate :: IO Integer
newNameplate = do
  entropy <- getEntropy 1
  let ch = head entropy
  let i = toInteger ch
  let n = i + 100
  return n

-- The ability to send an invoice through a Magic Wormhole.
class WormholeDelivery w where
  wormholeCodeGenerator :: w -> IO WormholeCode
  sendThroughWormhole   :: (MonadIO a, MonadThrow b) => w -> Text -> WormholeCode -> a (b ())

data WormholeServer =
  WormholeServer
  { wormholeServerRoot :: URL
  } deriving (Eq, Show)

data WormholeError =
  UnparseableWebSocketEndpoint
  deriving (Eq, Show, Exception)

instance WormholeDelivery WormholeServer where
  wormholeCodeGenerator ws = newWormholeCode

  -- Send an invoice along a Magic Wormhole.  Negotiate with the client first
  -- to make sure they will understand what we're going to send.
  -- TODO Really implement this
  sendThroughWormhole ws text wormholeCode = liftIO $ do
    TextIO.putStrLn $ "Sending " <> text
    let appID = MagicWormhole.AppID "tahoe-lafs.org/invite"
    let endpoint = exportURL $ wormholeServerRoot ws
    case MagicWormhole.parseWebSocketEndpoint endpoint of
      Nothing ->
        return $ throwM UnparseableWebSocketEndpoint
      Just wsEndpoint -> do
        side <- MagicWormhole.generateSide
        putStrLn $ show side
        MagicWormhole.runClient wsEndpoint appID side sendInvite
      where
        sendInvite session = do
          let spake2Password = makeSpake2Password wormholeCode

          putStrLn "Claiming nameplate"
          mailbox <- MagicWormhole.claim session $ MagicWormhole.Nameplate (pack . show . nameplate $ wormholeCode)
          putStrLn "Opening mailbox"
          peer <- MagicWormhole.open session mailbox
          putStrLn "Setting up encrypted connection handler"
          let send = MagicWormhole.withEncryptedConnection peer spake2Password interact
          putStrLn "Setting up final close"
          let close = MagicWormhole.close session (Just mailbox) Nothing
          putStrLn "Executing"
          finally send close
          return $ return ()

        sendJSON :: ToJSON a => a -> MagicWormhole.EncryptedConnection -> IO ()
        sendJSON obj conn = do
          let msg = MagicWormhole.Message $ decodeUtf8 $ toStrict $ encode obj
          MagicWormhole.sendMessage conn (MagicWormhole.PlainText (toStrict $ encode msg))

        interact :: MagicWormhole.EncryptedConnection -> IO ()
        interact = sendJSON ("Hello, world." :: Text)
