module Spec
  ( spec
  ) where

import Test.Hspec

spec :: Spec
spec =
  describe "no-op" $ do
  it "no-op" $ do
    () `shouldBe` ()
