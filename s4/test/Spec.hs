{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
  ( app
  , CreateSubscriptionResult(WormholeInvitation, InvalidPlanID)
  , CreateSubscription(CreateSubscriptionForPlan)
  )

import Wormhole
  ( WormholeCode
  , AbilitiesContainer(AbilitiesContainer)
  , Abilities(Abilities)
  , ClientV1(ClientV1)
  , ServerV1(ServerV1)
  , WormholeClient(sendSubscription)
  , NetworkWormholeClient(NetworkWormholeClient)
  , sendSubscription
  )

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
  ( bodyEquals
  )
import Test.Hspec.Wai.JSON
import Network.Wai.Test
  ( SResponse
  )
import Network.URI
  ( parseURI
  )
import Network.HTTP.Types
  ( Header,
    methodPost
  )
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Types.Status
  ( created201
  )
import Data.Aeson
  ( decode
  , encode
  )

main :: IO ()
main = hspec spec

data StubWormholeClient = StubWormholeClient WormholeCode

instance WormholeClient StubWormholeClient where
  sendSubscription (StubWormholeClient code) subscription =
    return $ Right (code, return $ Right ())

apiSpec :: Spec
apiSpec = with (return $ app $ StubWormholeClient "5-jumping-frogs") $ do
  describe "POST /v1/subscriptions" $ do
    let requestBody = encode $ CreateSubscriptionForPlan "abcd"
    it "responds with `Created`" $ do
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201
    it "responds with `Blubs`" $ do
      postJSON "/v1/subscriptions" (encode $ CreateSubscriptionForPlan "dcba") `shouldRespondWith` 403 { matchBody = bodyEquals $ encode InvalidPlanID }
    it "responds with application/json" $ do
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201
        { matchBody = hasHeader ("Content-Type", "application/json;charset=utf-8") }
    it "responds with a wormhole code" $ do
      let expectedBody = encode $ WormholeInvitation "5-jumping-frogs"
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201 { matchBody = bodyEquals expectedBody }

postJSON :: ByteString -> LB.ByteString -> WaiSession SResponse
postJSON path body = request methodPost path [("Content-Type", "application/json")] body

hasHeader :: Header -> MatchBody
hasHeader expected = MatchBody (\actual _ -> headerMatcher actual expected (show actual) (show expected))
  where
    headerMatcher [] expected actual' expected' = Just $ actualExpected "header not found" actual' expected'
    headerMatcher (aHeader:rest) expected actual' expected' =
      if aHeader == expected then
        Nothing
      else
        headerMatcher rest expected actual' expected'

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]

wormholeSpec :: Spec
wormholeSpec =
  describe "abilities serialization" $ do
  it "serializes client-v1" $
    let clientV1 = AbilitiesContainer (Abilities (Just ClientV1) Nothing)
        encodedForm = "{\"abilities\":{\"client-v1\":{}}}"
    in do
      encode clientV1 `shouldBe` encodedForm
      (decode encodedForm :: Maybe AbilitiesContainer) `shouldBe` Just clientV1

  it "serializes server-v1" $
    let serverV1 = AbilitiesContainer (Abilities Nothing (Just ServerV1))
        encodedForm = "{\"abilities\":{\"server-v1\":{}}}"
    in do
      encode serverV1 `shouldBe` encodedForm
      (decode encodedForm :: Maybe AbilitiesContainer) `shouldBe` Just serverV1


spec :: Spec
spec = do
  apiSpec
  wormholeSpec
