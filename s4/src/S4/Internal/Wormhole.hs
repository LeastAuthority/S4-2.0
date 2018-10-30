-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}

-- Make string literals the right type
{-# LANGUAGE OverloadedStrings #-}

module S4.Internal.Wormhole
  ( WormholeCode(WormholeCode)
  ) where

import Control.Monad
  ( fail
  )

import Data.Text
  ( Text
  , intercalate
  , pack
  , unpack
  , split
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , Value(String)
  , withText
  )

-- A structured representation of a Magic Wormhole code.
data WormholeCode =
-- The default number of passwords is two though Magic Wormhole allows this to
-- be configured on a per-wormhole basis.
  WormholeCode Nameplate [Password]
  deriving (Eq, Show)

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
