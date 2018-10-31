-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}

-- Make string literals the right type
{-# LANGUAGE OverloadedStrings #-}

module S4.Internal.Wormhole
  ( WormholeCode(WormholeCode)
  , WormholeDelivery(WormholeDelivery, wormholeCodeGenerator)
  , newWormholeCode
  , S4.Internal.Wormhole.deliverInvoice
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
  ( MonadThrow
  , throwM
  )

import Control.Concurrent.Async
  ( Async
  , async
  )

import Data.Map
  ( Map
  )

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
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , Value(String)
  , withText
  )

import Data.Text.PgpWordlist
  ( toText
  )

import System.Entropy
  ( getEntropy
  )

import S4.Internal.Invoice
  ( Invoice
  , deliverInvoice
  )

-- A structured representation of a Magic Wormhole code.
data WormholeCode =
-- The default number of passwords is two though Magic Wormhole allows this to
-- be configured on a per-wormhole basis.
  WormholeCode Nameplate [Password]
  deriving (Eq)

instance Show WormholeCode where
  show (WormholeCode nameplate password) = show nameplate <> "-" <> (unpack (intercalate "-" password))

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
data WormholeDelivery = WormholeDelivery
  { wormholeCodeGenerator :: IO WormholeCode
  }

deliverInvoice
  :: (MonadIO a, MonadThrow b)
  => WormholeDelivery
  -> Invoice
  -> a (b WormholeCode)
deliverInvoice (WormholeDelivery wormholeCodeGen) invoice = liftIO $ do
  wormholeCode <- wormholeCodeGen
  async $ do
      -- TODO Retry on errors where it makes sense - eg errors connecting to
      -- the Magic Wormhole server.  Propagate other errors to the
      -- subscription system, somehow, so the subscription associated with the
      -- invoice is marked as garbage (if this first invoice is not delivered
      -- then the legitimate owner of this subscription can never possibly use
      -- it; they should just retry).
    delivery <- S4.Internal.Invoice.deliverInvoice invoice wormholeCode
    case delivery of
      Left err -> throwM err
      Right () -> return ()

  return $ return wormholeCode
