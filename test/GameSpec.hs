{-# LANGUAGE ScopedTypeVariables #-}
module GameSpec (spec) where

import Game
import Player
import Test.Hspec

p0 :: Player = Player 0 "password" 0 True
p1 :: Player = Player 0 "password" (Player.maxFaliures - 1) True
p2 :: Player = Player 1 "password" 0 True
p3 :: Player = Player 2 "password" 0 False

testGame :: Game = Game 0 [('a', False)]
            [p1, p2]
            True
            p1
            "b"
testGame'  :: Game = Game 0 [('a', False)]
            [p1]
            True
            p1
            "b"
testGame'' :: Game = Game 0 [('a', False)]
            [p1, p3, p2]
            True
            p1
            "b"
testGame''' :: Game = Game 0 [('a', False)]
            [p1, p3]
            False
            p1
            "b"

spec :: Spec
spec = do
  describe "newGame" $ do
    it "rejects empty word" $
      newGame 0 "" `shouldBe` Nothing
    it "creates simple Game" $
      newGame 0 "a" `shouldBe` Just (Game 0  [('a', False)] [p0] True p0 "")
  describe "makeATurn" $ do
    it "rejrect a not running game" $
      makeATurn p1 'c' testGame''' `shouldBe` (testGame''', False)
    it "rejects a dead player" $
      makeATurn p3 'c' testGame `shouldBe` (testGame, False)
    it "reject a player not at turn" $
      makeATurn p2 'c' testGame `shouldBe` (testGame, False)
    it "makes a turn with killing mistake" $
      makeATurn p1 'c' testGame `shouldBe` (Game 0 [('a', False)] [Player 0 "password" Player.maxFaliures False, p2] True p2 "bc" ,True)
    it "ends game properly" $
      isRunning  (fst (makeATurn p1 'a' testGame)) `shouldBe` False
  describe "makeASolve" $ do
    it "rejects player not at turn" $
      makeASolve p2 "hi" testGame `shouldBe` (testGame, False)
    it "solves Game" $
      makeASolve p1 "a" testGame `shouldBe` (testGame {solution= [('a', True)], isRunning= False }, True)
  describe "nextPlayerAlive" $ do
    it "returns on regular game" $
      nextPlayerAlive testGame `shouldBe` p2
    it "returns yourself on singleplayer" $
      nextPlayerAlive testGame' `shouldBe` p1
    it "returns right player on complex Mulitplayer" $
      nextPlayerAlive testGame'' `shouldBe` p2
--  describe "updatePlayers" $
--    it "updates empty list" $
--        updatePlayers (1, "a", 1, True) [(0, "", 0, True), (1, "", 0, False)]
--          `shouldBe` [(0, "", 0, True), (1, "a", 1, True)]
