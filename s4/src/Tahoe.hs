{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Tahoe
  ( Configuration(StorageServers, storageFURLs, sharesNeeded, sharesTotal, sharesHappy, nickname)
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
  , Subscription(subscriptionStorage)
  , Storage(frontendAddress)
  )

type SharesNeeded = Integer
type SharesTotal = Integer
type SharesHappy = Integer
type Nickname = Text

data Configuration =
  StorageServers
  { storageFURLs   :: [FURL]
  , sharesNeeded   :: SharesNeeded
  , sharesTotal    :: SharesTotal
  , sharesHappy    :: SharesHappy
  , nickname       :: Nickname
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

fromSubscription :: Subscription -> Configuration
fromSubscription subscription =
  let furls servers = do
        server <- servers
        return $ frontendAddress server
  in
    StorageServers
    { storageFURLs = furls $ subscriptionStorage subscription
    , sharesNeeded = 1
    , sharesTotal = 1
    , sharesHappy = 1
    , nickname = "Least Authority S4 2.0"
    }
