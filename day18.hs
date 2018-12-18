#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (inRange)
import Data.List (foldl', sortOn, groupBy)
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = V2 Int
type Bounds = (Coord, Coord)
type World = Arr.Array Coord Cell
data Cell = Open | Wood | Yard deriving (Show, Eq, Ord)

main :: IO ()
main = do
    initialWorld <- parseInput <$> readFile "day18.input"
    print $ value $ nthEvolution initialWorld 10
    print $ value $ nthEvolution initialWorld 1000000000

nthEvolution :: World -> Int -> World
nthEvolution world n
  | n < 1000 = iterate tick world !! n
  | otherwise = iterate tick loopStartWorld !! loopMod
  where
    loopMod = (n - loopStartIndex) `mod` loopSize
    (loopStartWorld, loopStartIndex) = findLoop world (0, Set.empty)
    (_, loopSize) = findLoop loopStartWorld (0, Set.empty)

findLoop :: World -> (Int, Set World) -> (World, Int)
findLoop world (i, seen)
  | Set.member world seen = (world, i)
  | otherwise = findLoop world' (i + 1, Set.insert world seen)
  where world' = tick world

value :: World -> Int
value world = count Wood * count Yard
  where
    count cell = length $ filter (== cell) elems
    elems = Arr.elems world

tick :: World -> World
tick world = Arr.array (Arr.bounds world) assocs
  where
    assocs = newCell <$> Arr.assocs world
    newCell (c, Open) | adjacentCount c Wood >= 3 = (c, Wood)
                      | otherwise                 = (c, Open)
    newCell (c, Wood) | adjacentCount c Yard >= 3 = (c, Yard)
                      | otherwise                 = (c, Wood)
    newCell (c, Yard) | adjacentCount c Yard >= 1
                      , adjacentCount c Wood >= 1 = (c, Yard)
                      | otherwise                 = (c, Open)
    adjacentCount c cell = length (filter (== cell) (adjacencies world c))

adjacencies :: World -> Coord -> [Cell]
adjacencies world c = (world Arr.!) <$> validCoords
  where
    validCoords = filter (inRange (Arr.bounds world)) coords
    coords = (+ c) <$> [ V2 0 (-1), V2 (-1) 0, V2 1 0, V2 0 1
                       , V2 (-1) (-1), V2 (-1) 1, V2 1 (-1), V2 1 1
                       ]

pretty :: World -> String
pretty world = unlines ls
  where
    ls = fmap char <$> groupBy keySameRow (sortOn keyRow $ Arr.assocs world)
    char (_, Open) = '.'
    char (_, Wood) = '|'
    char (_, Yard) = '#'
    keyRow (V2 _ y, _) = y
    keySameRow a b = keyRow a == keyRow b

parseInput :: String -> World
parseInput input = Arr.array bounds assocs
  where
    bounds = (V2 0 0, V2 (length (head ls) - 1) (length ls - 1))
    assocs = do
        (y, line) <- zip [0..] ls
        (x, char) <- zip [0..] line
        return (V2 x y, cell char)
    cell '.' = Open
    cell '|' = Wood
    cell '#' = Yard
    ls = lines input
