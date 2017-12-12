{-# LANGUAGE ScopedTypeVariables #-}
module PlayerSpec (spec) where

import Player
import Test.Hspec

spec :: Spec
spec = do
  describe "new Player" $
    it "creates simple Player" $
        newPlayer 0 `shouldBe` (0, "", 0, True)
  describe "playersAlive" $ do
    it "filters empty list" $
        playersAlive [] `shouldBe` []
    it "filters simple list - true" $
        playersAlive [(0, "", 0, True)] `shouldBe` [(0, "", 0, True)]
    it "filters simple list - false" $
        playersAlive [(0, "", 0, False)] `shouldBe` []
    it "filters complex list" $
        playersAlive [(0, "", 0, True), (1, "", 0, False)] `shouldBe` [(0, "", 0, True)]
  describe "wrongGuess" $ do
    it "updates simple player" $
        wrongGuess (0, "", 0, True) `shouldBe` (0, "", 1, True)
    it "updates bad player" $
        wrongGuess (0, "", maxFaliures - 1, True) `shouldBe` (0, "", maxFaliures, False)
    it "let the dead rest" $
        wrongGuess (0, "", maxFaliures, False) `shouldBe` (0, "", maxFaliures, False)
