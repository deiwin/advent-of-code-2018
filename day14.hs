#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List (elemIndex)

type State = (Seq Int, Int, Int)

main :: IO ()
main = do
    let practiceCount = 190221
    let initialState = (Seq.fromList [3, 7], 0, 1)
    print $ goodRecipes practiceCount initialState
    print $ recipesToLeftOfSeq (digits practiceCount) initialState

recipesToLeftOfSeq :: Seq Int -> State -> Int
recipesToLeftOfSeq recipeQuery state
  | Just i <- subsetIndex recipeQuery s = i
  | otherwise = recipesToLeftOfSeq recipeQuery $ tick state
  where
    (s, _, _) = state

goodRecipes :: Int -> State -> String
goodRecipes practiceCount state
  | length s >= practiceCount + 10 = concat $ show <$> Seq.take 10 nonPracticeRecipes
  | otherwise = goodRecipes practiceCount $ tick state
  where
    (practiceRecipes, nonPracticeRecipes) = Seq.splitAt practiceCount s
    (s, _, _) = state

tick :: State -> State
tick (s, a, b) = (newS, newA, newB)
  where
    newS = s Seq.>< digits sum
    sum = Seq.index s a + Seq.index s b
    newA = move a
    newB = move b
    move x = (x + 1 + Seq.index s x) `mod` length newS

digits :: Int -> Seq Int
digits x
  | x < 10 = Seq.singleton x
  | otherwise = digits newX Seq.|> remainder
  where
    (newX, remainder) = x `divMod` 10

subsetIndex :: Seq Int -> Seq Int -> Maybe Int
subsetIndex small big = fmap (+ startIndex) $ elemIndex small $ chunks (length small) toCheck
  where
    chunks n s
      | length s < n = []
      | length s == n = [s]
      | otherwise = Seq.take n s : chunks n (Seq.drop 1 s)
    toCheck = Seq.drop startIndex big
    startIndex = length big - (length small + 2)
