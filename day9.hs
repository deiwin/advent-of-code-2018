#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Array.IArray (Array, elems, array, accum)
import Data.Ix (range)
import Data.List (repeat, zip, cycle)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))

type Scores = Array Int Int
type Circle = Seq Int

main :: IO ()
main = do
    input <- head . lines <$> readFile "day9.input"
    print $ uncurry solve $ parseLine input

solve :: Int -> Int -> Int
solve playerCount = play scores turns circle marble
  where
    scores = array bounds $ zip (range bounds) (repeat 0)
    bounds = (0, playerCount - 1)
    turns  = cycle [0 .. (playerCount - 1)]
    circle = Seq.singleton 0
    marble = 1

play :: Scores -> [Int] -> Circle -> Int -> Int -> Int
play scores (currentPlayer : nextPlayers) circle marble maxMarble
    | marble > maxMarble                = maxScore scores
    | fromIntegral marble `mod` 23 == 0 = play newScores nextPlayers circleWithoutScore nextMarble maxMarble
    | otherwise                         = play scores nextPlayers circleWithNewMarble nextMarble maxMarble
  where
    nextMarble                          = marble + 1
    circleWithNewMarble                 = addNewMarble circle marble
    newScores                           = accum (+) scores [(currentPlayer, marble), (currentPlayer, removedMarble)]
    (removedMarble, circleWithoutScore) = removeMarble (-7) circle

removeMarble :: Int -> Circle -> (Int, Circle)
removeMarble index circle
  | index < 0 = removeMarble (index + length circle) circle
  | otherwise = (removedMarble, end >< start)
  where
    (start, removedMarble :<| end) = Seq.splitAt index circle

addNewMarble :: Circle -> Int -> Circle
addNewMarble (x :<| y :<| rest ) newMarble = newMarble :<| rest >< Seq.fromList [x, y]
addNewMarble (x       :<| Empty) newMarble = Seq.fromList [newMarble, x]

maxScore :: Scores -> Int
maxScore = maximum . elems

parseLine :: String -> (Int, Int)
parseLine s = (playerCount, maxMarble)
  where
    playerCount = read $ head ws
    maxMarble   = read $ ws !! 6
    ws          = words s
