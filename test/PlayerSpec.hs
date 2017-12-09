{-# LANGUAGE ScopedTypeVariables #-}
module PlayerSpec (spec) where

import Player
import Test.Hspec

spec :: Spec
spec =
  describe "nothing" $
    it "does nothing" $
        True `shouldBe` True
