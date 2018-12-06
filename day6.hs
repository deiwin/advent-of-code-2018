#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package linear
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (all, elem, any, nub, sort)
import Linear.V2 (V2(..))
import Data.Array.IArray (Array, accumArray, bounds, (//), (!))
import Data.Ix (range, inRange)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type ID = Int
data Cell = Filled ID | Empty | Crowded deriving (Show, Eq)
isFilled (Filled _) = True
isFilled _          = False
type Matrix = Array Coord Cell

type Counts = Map ID Int

main :: IO ()
main = do
    input <- lines <$> readFile "day6.input"
    let coords = toV2 . fmap read . words . filter (/= ',') <$> input
    let filledM = untilFixed tick $ fillInitial coords
    let sideIDs = getSideIDs filledM
    print $ maximum $ Map.elems $ Map.filterWithKey (notIn sideIDs) $ getCounts filledM
  where
    notIn ids id _ = id `notElem` ids

getCounts :: Matrix -> Counts
getCounts m = Map.fromListWith (+) $ do
    i <- range $ bounds m
    let cell = m ! i
    guard $ isFilled cell
    let Filled id = cell
    return (id, 1)

getSideIDs :: Matrix -> [ID]
getSideIDs m = sort $ nub $ do
    i <- range b
    guard $ isSide i && isFilled (m ! i)
    let Filled id = m ! i
    return id
  where
    b = bounds m
    isSide coord = any (not . inRange b) $ surroundingCoords coord

tick :: Matrix -> Matrix
tick m = m // do
    i <- range $ bounds m
    guard $ (m ! i) == Empty
    return (i, newCell m i)

newCell :: Matrix -> Coord -> Cell
newCell m coord
  | cell /= Empty = cell
  | Crowded `elem` s = Crowded
  | otherwise = case length filledSurrounding of
                  0 -> Empty
                  1 -> head filledSurrounding
                  _ -> Crowded
  where
    cell = m ! coord
    s = surrounding m coord
    filledSurrounding = nub $ filter isFilled s

surrounding :: Matrix -> Coord -> [Cell]
surrounding m coord = do
    i <- surroundingCoords coord
    guard $ inRange (bounds m) i
    return $ m ! i

surroundingCoords :: Coord -> [Coord]
surroundingCoords coord = (coord +) . uncurry V2 <$> [(-1, 0), (0, -1), (1, 0), (0, 1)]

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x
  | newX == x = x
  | otherwise = untilFixed f newX
  where
    newX = f x

fillInitial :: [Coord] -> Matrix
fillInitial coords = accumArray (curry snd) Empty bounds assocs
  where
    bounds  = getBounds coords
    assocs  = zip coords idCells
    idCells = Filled <$> [0..]

getBounds :: [Coord] -> Bounds
getBounds coords = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    xs = getX <$> coords
    ys = getY <$> coords
    getX (V2 x _) = x
    getY (V2 _ y) = y

toV2 :: [Int] -> Coord
toV2 (x:y:_) = V2 x y
