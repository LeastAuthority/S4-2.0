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

type Nameplate = Integer
type Password = Text

data WormholeCode =
  WormholeCode Nameplate [Password]
  deriving (Eq, Show)

instance ToJSON WormholeCode where
  toJSON (WormholeCode nameplate passwords) = String $ pack (show nameplate) <> "-" <> (intercalate "-" passwords)

instance FromJSON WormholeCode where
  parseJSON = withText "WormholeCode" $ \s ->
    let
      parse (nameplate:passwords) = return $ WormholeCode (read (unpack nameplate) :: Nameplate) passwords
      parse otherwise = fail "glub"

      parts :: [Text]
      parts = split ((==) '-') s
    in
        parse parts
