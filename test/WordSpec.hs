{-# LANGUAGE ScopedTypeVariables #-}
module WordSpec (spec) where

import Word
import Test.Hspec

spec :: Spec
spec = do
  describe "createSolutionWord" $ do
    it "creates empty word" $
        createSolutionWord "" `shouldBe` []
    it "creates single char" $
        createSolutionWord "a" `shouldBe` [('a', False)]
    it "creates longer words" $
        createSolutionWord "abc" `shouldBe` [('a', False), ('b', False), ('c', False)]
  describe "tryChar" $ do
    it "ignores empty solution" $
        tryChar 'a' [] `shouldBe` False
    it "reject false single char" $
        tryChar 'a' [('b', False)] `shouldBe` False
    it "finds char in word" $
        tryChar 'a' [('a', False), ('b', False), ('a', False)] `shouldBe` True
  describe "solveChar" $ do
    it "solves empty solutions" $
        solveChar 'a' [] `shouldBe` []
    it "solves single char solution" $
        solveChar 'a' [('a', False)] `shouldBe` [('a', True)]
    it "solves complex unsolved word" $
        solveChar 'a' [('a', False), ('b', False), ('a', False)] `shouldBe` [('a', True), ('b', False), ('a', True)]
    it "solves complex partly solved word" $
        solveChar 'a' [('a', False), ('b', True), ('a', False)] `shouldBe` [('a', True), ('b', True), ('a', True)]
    it "doesn't solve solved solution" $
        solveChar 'a' [('a', True), ('b', True), ('a', True)] `shouldBe`  [('a', True), ('b', True), ('a', True)]
  describe "showSolution" $ do
    it "handles empty solution" $
        showSolution [] `shouldBe` []
    it "converts false chars to placeholder" $
        showSolution [('a', False)] `shouldBe` [Word.placeholder]
    it "converts longer word properly" $
        showSolution [('a', True), ('b', False), ('a', True)] `shouldBe` "a_a"
  describe "isPlayable" $ do
    it "handles empty solution" $
        isPlayable [] `shouldBe` False
    it "rejects solved solution" $
        isPlayable [('a', True), ('b', True), ('c', True)] `shouldBe` False
    it "confirms unsolved solution" $
        isPlayable [('a', True), ('b', False), ('c', True)] `shouldBe` True
  describe "solveWord" $ do
    it "shouldn't solve empty solutions" $
        solveWord "a" [] `shouldBe` (False, [])
    it "solves simple solution" $
        solveWord "ab" [('a', False), ('b', False)] `shouldBe` (True, [('a', True), ('b', True)])
