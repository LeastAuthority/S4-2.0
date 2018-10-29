-- Allow simple Aeson instance derivations
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module S4.Plan
  ( PlanID
  , Plan(Plan)
  ) where

import GHC.Generics
  ( Generic
  )

import Data.Text
  ( Text
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , genericToJSON
  , genericParseJSON
  )

import S4.Internal.JSON
  ( jsonOptions
  )

-- Uniquely identify a subscription plan.
type PlanID = Text

-- Describe a subscription plan for the service, including costs and benefits.
data Plan = Plan
  { planID       :: PlanID
  } deriving (Eq, Show, Generic)

instance ToJSON Plan where
  toJSON = genericToJSON jsonOptions

instance FromJSON Plan where
  parseJSON = genericParseJSON jsonOptions
