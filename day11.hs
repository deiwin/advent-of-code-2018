#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc

import Linear.V2 (V2(..))
import Data.Ix (range, inRange)
import Data.Array.IArray (Array, array, bounds, (!))
import Data.List (maximumBy, repeat, foldl1', scanl')
import Data.Ord (comparing)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type Grid = Array Coord Int

main :: IO ()
main = do
    print $ maxTotalPowerSquare 18
    print $ maxTotalPowerSquare 42
    print $ maxTotalPowerSquare 1133
    print $ maxTotalAnySizeSquare 1133

maxTotalAnySizeSquare :: Int -> ((Int, Int), Coord)
maxTotalAnySizeSquare serialNumber = maximumBy' (comparing maxF) possibleResults
  where
    maxF ((powerLevel, size), coord) = powerLevel
    possibleResults = zip (maxSquarePowerLevel grid <$> range bounds) (range bounds)
    bounds = (V2 1 1, V2 300 300)
    grid = buildGrid serialNumber bounds

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _   [] = error "List.maximumBy: empty list"
maximumBy' cmp xs = foldl1' maxBy xs
  where
    maxBy x y = case cmp x y of
        GT -> x
        _  -> y

maxTotalPowerSquare :: Int -> Coord
maxTotalPowerSquare serialNumber = maximumBy (comparing $ squarePowerLevel grid) $ range bounds
  where
    bounds = (V2 1 1, V2 300 300)
    grid = buildGrid serialNumber bounds

maxSquarePowerLevel :: Grid -> Coord -> (Int, Int)
maxSquarePowerLevel grid c = maximumBy' (comparing fst) $ drop 1 $ scanl' f (0, 0) [1 .. (maxSquareSize c)]
  where
    f (currentSum, _) size = (currentSum + borderSum size, size)
    borderSum size = sum $ (grid !) <$> borderCoords c size

maxSquareSize :: Coord -> Int
maxSquareSize (V2 x y) = 301 - max x y

borderCoords :: Coord -> Int -> [Coord]
borderCoords (V2 x y) size = uncurry V2 <$> coords
  where
    coords = zip [x .. (end x)] (repeat (end y)) ++ zip (repeat (end x)) [y .. (end y - 1)]
    end a = a + size - 1

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
