{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List
  ( isInfixOf
  )
import Data.Text
  ( unpack
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Data.ByteString.Lazy
  ( toStrict
  )
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

import Network.Wai.Test
  ( SResponse(simpleBody)
  )

import S4.Internal.API
  ( CreateSubscriptionResult(WormholeInvitation)
  , app
  )
import S4.Internal.Wormhole
  ( WormholeCode(WormholeCode)
  , newWormholeCode
  )
import S4.Plan (Plan)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  wormholeSpec
  httpSpec

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


httpSpec :: Spec
httpSpec = with (return app) $ do
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
          -- TODO: Parameterize the code generator in the application
          expected :: WormholeCode
          expected = WormholeCode 101 ["monoidal", "endofunctors"]

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
        createSubscription "{\"create-for-plan-id\": \"abcd\"}" `shouldRespondWith` matchIfBody 201 (aWormholeInvitation expected)
