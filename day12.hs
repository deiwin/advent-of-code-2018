#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldr, iterate, zipWith)

type State = String
type SpreadingNotes = Map String Char

main :: IO ()
main = do
    (initialState, notes) <- parseInput <$> readFile "day12.input"
    print $ sumAfter20 notes initialState

sumAfter20 :: SpreadingNotes -> State -> Int
sumAfter20 notes initialState = sum $ zipWith plantIndex result [startingIndex..]
  where
    plantIndex '.' _ = 0
    plantIndex '#' i = i
    result = iterate (tick notes) initialState !! iterations
    startingIndex = iterations * (-4)
    iterations = 20

tick :: SpreadingNotes -> State -> State
tick notes state = snd $ foldr (update notes) (buffer, buffer) (buffer ++ state)
    where buffer = "...."

update :: SpreadingNotes -> Char -> (State, State) -> (State, State)
update notes a (old, new) = (updatedOld, updatedNew)
  where
    updatedOld = a:old
    updatedNew = a:bn:newChar:restn
    newChar = notes Map.! [a, b, c, d, e]
    (b:c:d:e:_) = old
    (bn:_:restn) = new

parseInput :: String -> (State, SpreadingNotes)
parseInput input = (initialState, notes)
  where
    initialState = words (head l) !! 2
    notes = Map.fromList $ toNote . words <$> drop 2 l
    toNote (s:_:c:_) = (s, head c)
    l = lines input
