{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Exception.Safe
  ( SomeException
  , Exception
  , throwM
  )

import Data.Either.Combinators
  ( rightToMaybe
  )

import Data.List
  ( isInfixOf
  , nub
  )

import Data.Text
  ( Text
  , unpack
  )

import Data.Text.Encoding
  ( decodeUtf8
  )

import qualified Data.ByteString.UTF8 as UTF8

import Data.ByteString.Lazy
  ( toStrict
  )

import qualified Data.ByteString.Base64.URL as B64U

import Data.Aeson
  ( decode
  , encode
  , eitherDecode
  )

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Network.HTTP.Types
  ( Header
  , methodPost
  )

import Network.URL
  ( URL(URL, url_type, url_path, url_params)
  , importURL
  )

import Network.Wai.Test
  ( SResponse(simpleBody)
  )

import S4.Internal.API
  ( CreateSubscriptionResult(WormholeInvitation)
  , app
  )

import S4.Internal.Deployment
  ( Deployment(Deployment, wormholeDelivery)
  )

import S4.Internal.Invoice
  ( Invoice
  , invoice
  , toText
  )

import S4.Internal.Wormhole
  ( WormholeDelivery(wormholeCodeGenerator, sendThroughWormhole)
  , WormholeServer
  , WormholeCode(WormholeCode)
  , newWormholeCode
  )

import S4.Plan (Plan)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  wormholeSpec
  httpSpec
  invoiceSpec

wormholeSpec :: Spec
wormholeSpec =
  let
    s = "\"101-monoidal-endofunctors\""
    c = WormholeCode 101 ["monoidal", "endofunctors"]
  in
    describe "WormholeCode" $ do
      it "is JSON encodable" $
        encode c `shouldBe` s

      it "is JSON decodeable" $
        decode s `shouldBe` Just c

      -- TODO Test more bad inputs
      it "fails with a useful error" $
        (eitherDecode "\"1\"" :: Either String WormholeCode) `shouldSatisfy` (\(Left err) -> "WormholeCode not parsed from 1" `isInfixOf` err)

      it "shows usefully" $
        show c `shouldBe` "101-monoidal-endofunctors"

      context "generation" $ do
        it "generates two-word codes" $ do
          WormholeCode nameplate password <- newWormholeCode
          length password `shouldBe` 2

        it "generates positive integer nameplates" $ do
          WormholeCode nameplate password <- newWormholeCode
          nameplate `shouldSatisfy` (flip (>) 0)


data FakeWormhole = FakeWormhole WormholeCode

instance WormholeDelivery FakeWormhole where
  wormholeCodeGenerator (FakeWormhole code) = return code

  sendThroughWormhole (FakeWormhole expectedCode) text wormholeCode =
    if expectedCode == wormholeCode then
      return $ return ()
    else
      return $ throwM WrongWormholeCode

data WrongWormholeCode = WrongWormholeCode deriving (Eq, Show, Exception)

-- Spec for the HTTP API
httpSpec :: Spec
httpSpec =
  let
    wormholeCode = WormholeCode 101 ["monoidal", "endofunctors"]
    deployment = Deployment { wormholeDelivery = FakeWormhole wormholeCode }
  in
    httpSpec' wormholeCode deployment

-- Helper to actually generate the spec.
httpSpec'
  :: WormholeDelivery w
  => WormholeCode  -- The constant code that can be expected using the given
                   -- deployment.
  -> Deployment w  -- The Deployment to use to construct the server.
  -> Spec
httpSpec' wormholeCode deployment = with (return $ app deployment) $ do
  let
    matchIfBody status pred = status { matchBody = MatchBody pred }

  describe "GET /v1/plans" $ do
    it "responds with 200" $ do
      get "/v1/plans" `shouldRespondWith` 200
    it "responds with [Plan]" $ do
      let
        somePlans headers body =
          case decode body :: Maybe [Plan] of
            Nothing -> Just "Failed to deserialize body to [Plan]"
            otherwise -> Nothing
      get "/v1/plans" `shouldRespondWith` matchIfBody 200 somePlans

  describe "POST /v1/subscriptions" $ do
    let createSubscription =
          request methodPost "/v1/subscriptions" [("Content-Type", "application/json")]
    it "responds with 415 to non-JSON" $ do
      post "/v1/subscriptions" "anything" `shouldRespondWith` 415
    it "responds with 400 to unparseable JSON" $ do
      createSubscription "anything" `shouldRespondWith` 400
    it "responds with 403 for a plan which does not exist" $ do
      createSubscription "{\"create-for-plan-id\": \"wxyz\"}" `shouldRespondWith` 403
    context "for a plan id identifying a real plan" $ do
      it "responds with 201" $ do
        createSubscription "{\"create-for-plan-id\": \"abcd\"}" `shouldRespondWith` 201
      it "responds with a Magic Wormhole invitation" $ do
        let
          aWormholeInvitation :: WormholeCode -> [Header] -> Body -> Maybe String
          aWormholeInvitation expected headers body =
            case decode body :: Maybe CreateSubscriptionResult of
              Nothing ->
                let textBody = decodeUtf8 $ toStrict body
                    stringBody = unpack textBody
                in
                  Just ("Failed to deserialize body to WormholeCode: " <> stringBody)
              Just (WormholeInvitation actual) ->
                if expected == actual then
                  Nothing
                else
                  Just (show expected <> " != " <> show actual)
        createSubscription "{\"create-for-plan-id\": \"abcd\"}" `shouldRespondWith` matchIfBody 201 (aWormholeInvitation wormholeCode)

-- Spec for invoices
invoiceSpec :: Spec
invoiceSpec = do
  describe "toText" $ do
    context "URI query arguments" $
      let
        Just (URL { url_type, url_path, url_params }) = importURL . unpack $ (toText invoice)
      in do
        it "has unique query arguments" $
          let
            paramKeys = map (\(k, v) -> k) url_params
          in
            paramKeys `shouldBe` nub paramKeys

        it "has a urlsafe-base64-encoded service label" $
          let
            label = (lookup "l" url_params) >>= (rightToMaybe . B64U.decode . UTF8.fromString)
          in
            label `shouldBe` Just "Least Authority S4 2.0"

  describe "deliverInvoice" $ do
    it "does stuff" $ do
      1 `shouldBe` 1
