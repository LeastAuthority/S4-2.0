{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
  ( app
  , CreateSubscriptionResult(WormholeInvitation, InvalidPlanID)
  , CreateSubscription(CreateSubscriptionForPlan)
  )

import qualified Invoice

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
import Test.Hspec.Wai hiding
  ( pending
  )
import Test.Hspec.Wai.Matcher
  ( bodyEquals
  )
import Test.Hspec.Wai.JSON
import Network.Wai.Test
  ( SResponse
  )
import Network.URI
  ( parseURI
  , uriToString
  )

import qualified Crypto.Saltine.Class as Saltine
import Network.HTTP.Types
  ( Header,
    methodPost
  )
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.UTF8
  ( toString
  , fromString
  )

import Network.HTTP.Types.Status
  ( created201
  )
import Data.Aeson
  ( decode
  , encode
  )

import Data.Time.Format
  ( parseTimeM
  , defaultTimeLocale
  )

import Data.URLEncoded
  ( importString
  , lookupAll
  )

import Model
  ( Subscription
  , Currency(ZEC)
  , Plan(Plan)
  , SigningKey(SigningKey)
  , signingKey
  , publicKey
  , paymentStatus
  , newSubscription
  , nextInvoiceURL
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


invoiceSpec :: Spec
invoiceSpec =
  let
    plan = Plan "abcd" (25 * 3600) ZEC (read "0.1")
    now = parseTimeM False defaultTimeLocale "%FT%T" "2018-10-30T14:30:45"
    get' :: String -> Subscription -> [String]
    get' field subscription =
      let
        encoded = Invoice.encode subscription
        Just decoded = importString encoded
      in
        lookupAll field decoded
    get field = do
      subscription <- newSubscription now plan
      return $ get' field subscription
    shouldBe' = flip shouldBe
  in
    describe "serialization" $ do
    it "v is version 1" $
      get ("v" :: String) >>= (shouldBe' ["1"])
    it "c specifies ZEC" $
      get ("c" :: String) >>= (shouldBe' ["ZEC"])
    it "a gives the amount" $
      get ("a" :: String) >>= (shouldBe' ["0.1"])
    it "d gives the due date" $
      get ("d" :: String) >>= (shouldBe' ["2018-10-31T15:30:45"])
    it "e gives the date the subscription is good for after the next payment" $
      get ("e" :: String) >>= (shouldBe' ["2018-11-01T16:30:45"])
    it "l is base64 encoded" $
      get ("l" :: String) >>= (shouldBe' [toString $ B64U.encode "Least Authority S4 / P4"])
    it "u is base64 encoded" $ do
      subscription <- newSubscription now plan
      let value = get' ("u" :: String) subscription
      let decoded = map (B64U.decode . fromString) value
      let expectedURL = nextInvoiceURL subscription
      let expectedDecoded = fromString $ (uriToString id expectedURL) ""
      decoded `shouldBe` [Right expectedDecoded]
    it "r gives the credit" $
      get "r" >>= (shouldBe' ["0.0"])
    it "p gives the public key" $ do
      subscription <- newSubscription now plan
      let (SigningKey (secretKey, publicKey)) = signingKey . paymentStatus $ subscription
      let actual = get' "p" subscription
      [toString . B64U.encode . Saltine.encode $ publicKey] `shouldBe` actual
    it "m gives the Tahoe-LAFS configuration" $
      pending


spec :: Spec
spec = do
  apiSpec
  wormholeSpec
  invoiceSpec
