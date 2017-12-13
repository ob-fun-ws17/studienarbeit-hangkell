{-|
Module      : Hangman Word Module
Description : Library to handle a hangman solution word.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Word where

import Data.Char(toLower)
import Data.List

-- | Placeholder for unsolved chars in solution
placeholder = '_' :: Char -- Deprecated? :D

-- | Solution of a Hangman game. Unguessd chars are marked with a False.
type SolutionWord = [(Char, Bool)]

{- | Creates a SolutionWord from a given String

>>> createSolutionWord "abc"
[('a', False), ('b', False), ('b', False)]
-}
createSolutionWord ::
  String -- ^ String to play with
  -> SolutionWord -- ^ Solution to create a game with
createSolutionWord = solveChar '.' . solveChar ',' . solveChar '!' . solveChar '?' . solveChar ';' . solveChar '\'' . solveChar ' ' . map (\char -> (char, False))

{- | Checks if a given char is in the word.

>>> tryChar 'a' "Hangman"
True
-}
tryChar ::
  Char -- ^ The char the player played
  -> SolutionWord -- ^ The word that is being played3
  -> Bool -- ^ wether guessed char was right
tryChar try solution = not . null $ elemIndices try (map fst solution)

{- | Applys a guess on the solution.

>>> solveChar a [('a', false), ('b', true)]
[('a', True), ('b', True)]
-}
solveChar ::
  Char -- ^ Char that player tries to add
  -> SolutionWord -- ^ Current game state
  -> SolutionWord -- ^ New state of progress
solveChar try = map (\(char, state) -> (char, state || toLower char == toLower try))

{- | Converts a solution into a String

>>> showSolution [('a', True), ('b', False)]
"a_"
-}
showSolution ::
  SolutionWord -- ^ SolutionWord to show
  -> String -- ^ Stringified version of the solution
showSolution = map (\sol -> if snd sol then fst sol else placeholder)

{- | Checks if there are unknown chars in a solutionWord left.

>>> isPlayable [('a', True)]
False
-}
isPlayable ::
  SolutionWord -- ^ Word to check
  -> Bool -- ^ Wether the word is still playable
isPlayable word = not . null $ filter (not . snd) word
