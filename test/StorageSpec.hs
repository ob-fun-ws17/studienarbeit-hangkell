{-# LANGUAGE ScopedTypeVariables #-}
module StorageSpec (spec) where

import Game
import Storage
import Data.Maybe (fromJust)
import Test.Hspec

g0 = fromJust $ newGame 0 "a"
g1 = fromJust $ newGame 1 "b"

spec :: Spec
spec =
  describe "updateGames" $ do
    it "appends to empty list" $
      updateGames [] g0 `shouldBe` [g0]
    it "appends to simple list" $
      updateGames [g0] g1 `shouldBe` [g0,g1]
    it "overrides simple list" $
      updateGames [g0] g0 `shouldBe` [g0]
    it "overrides complex list" $
      updateGames [g0,g1] g0 `shouldBe` [g0,g1]
