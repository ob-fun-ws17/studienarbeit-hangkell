{-# LANGUAGE ScopedTypeVariables #-}
module GameSpec (spec) where

import Game
import Test.Hspec

spec :: Spec
spec = do
  describe "nothing" $
    it "creates simple Player" $
        False `shouldBe` False
  describe "updatePlayers" $ 
    it "updates empty list" $
        updatePlayers (1, "a", 1, True) [(0, "", 0, True), (1, "", 0, False)]
          `shouldBe` [(0, "", 0, True), (1, "a", 1, True)]
