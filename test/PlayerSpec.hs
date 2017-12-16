{-# LANGUAGE ScopedTypeVariables #-}
module PlayerSpec (spec) where

import Player
import Test.Hspec

-- S.U.T.
p1 :: Player = Player 0 "" 0 True
p1' :: Player = Player 0 "" 1 True
p2 :: Player = Player 1 "" (maxFaliures - 1) True
p2' :: Player = Player 1 "" maxFaliures False

spec :: Spec
spec = do
  describe "new Player" $
    it "creates simple Player" $
        newPlayer 0 `shouldBe` p1
  describe "playersAlive" $ do
    it "filters empty list" $
        playersAlive [] `shouldBe` []
    it "filters simple list - true" $
        playersAlive [p1] `shouldBe` [p1]
    it "filters simple list - false" $
        playersAlive [p2'] `shouldBe` []
    it "filters complex list" $
        playersAlive [p1,p2'] `shouldBe` [p1]
  describe "wrongGuess" $ do
    it "updates simple player" $
        wrongGuess p1 `shouldBe` p1'
    it "updates bad player" $
        wrongGuess p2 `shouldBe` p2'
    it "let the dead rest" $
        wrongGuess p2' `shouldBe` p2'
