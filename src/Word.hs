{-|
Module      : Hangman Word Module
Description : Library to handle a hangman solution word.
-}
module Word
    (tryChar, solveChar) where

import Data.Char(toLower)
import Data.List

-- | Solution of a Hangman game. Unguessd chars are marked with a False.
type SolutionWord = [(Char, Bool)]

{- | Creates a SolutionWord from a given String

>>> createSolutionWord "abc"
[('a', false), ('b', false), ('b', false)]
-}
createSolutionWord ::
  String -- ^ String to play with
  -> SolutionWord -- ^ Solution to create a game with
createSolutionWord = map (\char -> (char, False))

{- | Checks if a given char is in the word.

>>> tryChar 'a' "Hangman"
True
-}
tryChar ::
  Char -- ^ The char the player played
  -> SolutionWord -- ^ The word that is being played3
  -> Bool -- ^ wether guessed char was right
tryChar try solution = null $ elemIndices try (map fst solution)

{- | Applys a guess on the solution.

>>> solveChar a [('a', false), ('b', true)]
[('a', True), ('b', True)]
-}
solveChar ::
  Char -- ^ Char that player tries to add
  -> SolutionWord -- ^ Current game state
  -> SolutionWord -- ^ New state of progress
solveChar try = map (\(char, state) -> (char, state || toLower char == toLower try))
