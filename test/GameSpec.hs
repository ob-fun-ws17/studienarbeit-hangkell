{-# LANGUAGE ScopedTypeVariables #-}
module GameSpec (spec) where

import Game
import Player
import Test.Hspec

p1 :: Player = (0, "", Player.maxFaliures - 1, True)
p2 :: Player = (1, "", 0, True)
p3 :: Player = (2, "", 0, False)

testGame :: Game = ([('a', False)],
            [p1, p2],
            True,
            p1,
            "b"
          )
testGame'  :: Game = ([('a', False)],
            [p1],
            True,
            p1,
            "b"
          )
testGame'' :: Game = ([('a', False)],
            [p1, p3, p2],
            True,
            p1,
            "b"
          )
testGame''' :: Game = ([('a', False)],
            [p1, p3],
            False,
            p1,
            "b"
          )

spec :: Spec
spec = do
  describe "newGame" $ do
    it "rejects empty word" $
      newGame "" `shouldBe` Nothing
    it "creates simple Game" $
      newGame "a" `shouldBe` Just ([('a', False)], [(0, "", 0, True)], True, (0, "", 0, True), "")
  describe "makeATurn" $ do
    it "rejrect a not running game" $
      makeATurn p1 'c' testGame''' `shouldBe` (testGame''', False)
    it "rejects a dead player" $
      makeATurn p3 'c' testGame `shouldBe` (testGame, False)
    it "reject a player not at turn" $
      makeATurn p2 'c' testGame `shouldBe` (testGame, False)
    it "makes a turn with killing mistake" $
      makeATurn p1 'c' testGame `shouldBe` (([('a', False)],[(0, "", Player.maxFaliures, False), p2],True,p2,"bc"),True)
  describe "playerAtTurn" $
    it "returns right player" $
      playerAtTurn testGame `shouldBe` Just (0, "", Player.maxFaliures - 1, True)
  describe "nextPlayerAlive" $ do
    it "returns on regular game" $
      nextPlayerAlive testGame `shouldBe` (1, "", 0, True)
    it "returns yourself on singleplayer" $
      nextPlayerAlive testGame' `shouldBe` (0, "", Player.maxFaliures - 1, True)
    it "returns right player on complex Mulitplayer" $
      nextPlayerAlive testGame'' `shouldBe` (1, "", 0, True)
--  describe "updatePlayers" $
--    it "updates empty list" $
--        updatePlayers (1, "a", 1, True) [(0, "", 0, True), (1, "", 0, False)]
--          `shouldBe` [(0, "", 0, True), (1, "a", 1, True)]
