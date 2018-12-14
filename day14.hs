#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type State = (Seq Int, Int, Int)

main :: IO ()
main = do
    let practiceCount = 190221
    print $ goodRecipes practiceCount (Seq.fromList [3, 7], 0, 1)

goodRecipes :: Int -> State -> String
goodRecipes practiceCount (s, a, b)
  | length s >= practiceCount + 10 = concat $ show <$> Seq.take 10 nonPracticeRecipes
  | otherwise = goodRecipes practiceCount (newS, newA, newB)
  where
    (practiceRecipes, nonPracticeRecipes) = Seq.splitAt practiceCount s
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
