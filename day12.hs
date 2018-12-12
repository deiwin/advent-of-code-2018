#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldr, zip)

type State = Set Int
type Notes = Map [Bool] Bool

main :: IO ()
main = do
    (initialState, notes) <- parseInput <$> readFile "day12.input"
    print $ sumAfterN notes initialState 20
    print $ sumAfterN notes initialState 50000000000

sumAfterN :: Notes -> State -> Int -> Int
sumAfterN notes !state iterations | iterations > stableStart = sum + (iterations - stableStart) * diff
    where (stableStart, sum, diff) = stableIterationStart notes state 0 0 0
sumAfterN notes !state 0          = sum state
sumAfterN notes !state iterations = sumAfterN notes (tick notes state) (iterations - 1)

stableIterationStart :: Notes -> State -> Int -> Int -> Int -> (Int, Int, Int)
stableIterationStart notes !state 5 stableDiff iterations = (iterations, sum state, stableDiff)
stableIterationStart notes !state stableIterations stableDiff iterations
    | newDiff == stableDiff = stableIterationStart notes newState (stableIterations + 1) stableDiff (iterations + 1)
    | otherwise             = stableIterationStart notes newState 0 newDiff (iterations + 1)
  where
    newState = tick notes state
    newDiff  = sum newState - sum state

tick :: Notes -> State -> State
tick notes state = Set.foldr f state state
  where
    f i newState = foldr ($) newState $ insertDelete <$> surroundingIndices i
    insertDelete i = if willHavePlant notes state i
                        then Set.insert i
                        else Set.delete i

willHavePlant :: Notes -> State -> Int -> Bool
willHavePlant notes state index = notes Map.! surroundingState
  where
    surroundingState = (`Set.member` state) <$> surroundingIndices index

surroundingIndices :: Int -> [Int]
surroundingIndices i = [(i - 2) .. (i + 2)]

parseInput :: String -> (State, Notes)
parseInput input = (initialState, notes)
  where
    initialState = Set.fromList $ fmap fst $ filter (isPlant . snd) $ zip [0..] (words (head l) !! 2)
    notes = Map.fromList $ toNote . words <$> drop 2 l
    toNote (s:_:c:_) = (isPlant <$> s, isPlant (head c))
    isPlant = (== '#')
    l = lines input
