{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
  ( app
  , CreateSubscriptionResult(WormholeInvitation)
  , CreateSubscription(CreateSubscriptionForPlan)
  )
import Wormhole
  ( WormholeCode
  , WormholeClient(sendSubscription)
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
import Network.HTTP.Types
  (Header,  methodPost
  )
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Types.Status
  ( created201
  )
import Data.Aeson
  ( encode
  )

main :: IO ()
main = hspec spec

data StubWormholeClient = StubWormholeClient WormholeCode

instance WormholeClient StubWormholeClient where
  sendSubscription (StubWormholeClient code) subscription =
    return $ Right (code, return $ Right ())

spec :: Spec
spec = with (return $ app $ StubWormholeClient "5-jumping-frogs") $ do
  describe "POST /v1/subscriptions" $ do
    let requestBody = encode $ CreateSubscriptionForPlan "abcd"
    it "responds with `Created`" $ do
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201
    it "responds with application/json" $ do
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201
        { matchBody = hasHeader ("Content-Type", "application/json;charset=utf-8") }
    it "responds with a wormhole code" $ do
      let expectedBody = encode $ WormholeInvitation "5-jumping-frogs"
      postJSON "/v1/subscriptions" requestBody `shouldRespondWith` 201 { matchBody = bodyEquals expectedBody }
  where
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
