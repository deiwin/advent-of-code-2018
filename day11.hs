#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc

import Linear.V2 (V2(..))
import Data.Ix (range, inRange)
import Data.Array.IArray (Array, array, bounds, (!))
import Data.List (maximumBy)
import Data.Ord (comparing)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type Grid = Array Coord Int

main :: IO ()
main = do
    print $ maxTotalPowerSquare 18
    print $ maxTotalPowerSquare 42
    print $ maxTotalPowerSquare 1133

maxTotalPowerSquare :: Int -> Coord
maxTotalPowerSquare serialNumber = maximumBy (comparing $ squarePowerLevel grid) $ range bounds
  where
    bounds = (V2 1 1, V2 300 300)
    grid = buildGrid serialNumber bounds

squarePowerLevel :: Grid -> Coord -> Int
squarePowerLevel grid c
  | inRange b end = sum $ (grid !) <$> range (c, end)
  | otherwise = minBound
  where
    b = bounds grid
    end = c + V2 2 2

buildGrid :: Int -> Bounds -> Grid
buildGrid serialNumber bounds = array bounds $ zip coords (powerLevel serialNumber <$> coords)
    where coords = range bounds

powerLevel :: Int -> Coord -> Int
powerLevel serialNumber (V2 x y) = hundreds ((rackID * y + serialNumber) * rackID) - 5
    where rackID = x + 10

hundreds :: Int -> Int
hundreds x
  | x < 100 = 0
  | otherwise = (x `div` 100) `mod` 10
