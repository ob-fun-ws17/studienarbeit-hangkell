{-# LANGUAGE ScopedTypeVariables #-}
module PlayerSpec (spec) where

import Player
import Test.Hspec

-- S.U.T.
p1 :: Player = Player 0 "a" 0 True
p1' :: Player = Player 0 "a" 1 True
p2 :: Player = Player 1 "b" (maxFaliures - 1) True
p2' :: Player = Player 1 "b" maxFaliures False

spec :: Spec
spec = do
  describe "new Player" $
    it "creates simple Player" $
        playerId (newPlayer 0) `shouldBe` 0
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
  describe "getPlayerForID" $ do
    it "ignores empty player sets" $
        getPlayerForId [] 0 "a" `shouldBe` Nothing
    it "ignores invalid player ids" $
        getPlayerForId [p1] (-1) "a" `shouldBe` Nothing
    it "ignores empty keys" $
        getPlayerForId [p1] 0 "" `shouldBe` Nothing
    it "rejects wrong keys" $
        getPlayerForId [p1] 0 "wrong" `shouldBe` Nothing
    it "returns player with right key" $
        getPlayerForId [p1] 0 "a" `shouldBe` Just p1
    it "returns nothing if there is no one to return" $
        getPlayerForId [p1] 1 "b" `shouldBe` Nothing
