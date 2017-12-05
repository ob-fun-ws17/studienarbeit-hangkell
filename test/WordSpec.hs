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
  describe "showSolution" $ do
    it "handles empty solution" $
        showSolution [] `shouldBe` []
    it "converts false chars to placeholder" $
        showSolution [('a', False)] `shouldBe` [Word.placeholder]
