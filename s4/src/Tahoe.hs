{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Tahoe
  ( Configuration
  , fromSubscription
  ) where

import GHC.Generics
  ( Generic
  )

import Data.Text
  ( Text
  )

import Data.Aeson
  ( ToJSON
  , FromJSON
  )

import Model
  ( FURL
  , Subscription
  )

type SharesNeeded = Integer
type SharesTotal = Integer
type SharesHappy = Integer
type Nickname = Text

data Configuration =
  StorageServers [FURL] SharesNeeded SharesTotal SharesHappy Nickname
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

fromSubscription :: Subscription -> Configuration
fromSubscription subscription =
  StorageServers [] 1 1 1 "you let me set your nickname"
