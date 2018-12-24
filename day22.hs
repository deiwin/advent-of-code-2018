#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package astar
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (range, inRange)
import Data.List ((!!), span, foldl')
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import GHC.Generics (Generic)
import Data.Hashable

type Coord = V2 Int
type ErosionMap = Arr.Array Coord Int

data Type = Rocky | Wet | Narrow deriving (Show)
data Tools = Climbing | Torch | Neither deriving (Show, Eq, Ord, Generic)
instance Hashable Tools

main :: IO ()
main = do
    (depth, targetCoord) <- parseInput <$> readFile "day22.input"
    let emap = mkErosionMap targetCoord depth
    print $ calculateTotalRisk targetCoord emap
    print $ calculateTime targetCoord emap

calculateTotalRisk :: Coord -> ErosionMap -> Int
calculateTotalRisk targetCoord emap = sum $ (`mod` 3) . (emap Arr.!) <$> range (V2 0 0, targetCoord)

calculateTime :: Coord -> ErosionMap -> Maybe Int
calculateTime targetCoord emap = (+ 1) . totalDist <$> path
  where
    totalDist cs = snd $ foldl' f (Nothing, 0) cs
    f (Nothing, s) c' = (Just c', s)
    f (Just c, s) c' = (Just c', s + dist c c')
    path = aStar neighbours dist heuristic isDest start
    start = (V2 0 0, Torch)
    neighbours (c, tool) = toolChange c tool `HS.union` possibleNeighbours c tool
    toolChange c tool = HS.fromList $ (c, ) <$> filter (/= tool) (availableTools emap c)
    possibleNeighbours c tool =
        HS.fromList
        $ filter ((== tool) . snd)
        $ concatMap (\c' -> (c', ) <$> availableTools emap c') $ adjacencies c
    dist (c, t) (c', t')
      | c == c' = 7
      | otherwise = 1
    heuristic (c, t) = mDist (targetCoord - c)
    mDist (V2 x y) = abs x + abs y
    isDest (c, tool) = c == targetCoord && tool == Torch
    adjacencies c = filter (inRange b) $ (c +) <$> [V2 0 (- 1), V2 (- 1) 0, V2 1 0, V2 0 1]
    b = Arr.bounds emap

availableTools :: ErosionMap -> Coord -> [Tools]
availableTools emap c = options $ terrainType (emap Arr.! c)
  where
    options Rocky = [Climbing, Torch]
    options Wet = [Climbing, Neither]
    options Narrow = [Torch, Neither]

terrainType :: Int -> Type
terrainType x
  | r == 0 = Rocky
  | r == 1 = Wet
  | otherwise = Narrow
  where r = x `rem` 3

mkErosionMap :: Coord -> Int -> ErosionMap
mkErosionMap targetCoord depth = emap
  where
    emap = Arr.array bounds $ (\c -> (c, calcGeoIx c)) <$> range bounds
    get = (emap Arr.!)
    calcGeoIx c = gmod (calcGeoIx' c + depth)
    calcGeoIx' c@(V2 x y)
      | c == V2 0 0 = 0
      | c == targetCoord = 0
      | y == 0 = gmod (gmod x * 16807)
      | x == 0 = gmod (gmod y * gmod 48271)
      | otherwise = gmod (gmod (get (V2 (x - 1) y)) * gmod (get (V2 x (y - 1))))
    bounds = (V2 0 0, buffered targetCoord)
    buffered (V2 x y) = V2 (100 * x) (10 * y)

gmod x = x `rem` 20183

parseInput :: String -> (Int, Coord)
parseInput input = (depth, targetCoord)
  where
    depth                        = read (words depthLine !! 1)
    targetCoord                  = V2 (read x) (read y)
    (x, _ : y)                   = span (/= ',') (words targetLine !! 1)
    (depthLine : targetLine : _) = lines input
