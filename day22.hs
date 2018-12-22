#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (range)
import Data.List ((!!), span, foldl')

type Coord = V2 Int
type ErosionMap = Arr.Array Coord Int

main :: IO ()
main = do
    (depth, targetCoord) <- parseInput <$> readFile "day22.input"
    print $ calculateTotalRisk $ createErosionMap targetCoord depth

calculateTotalRisk :: ErosionMap -> Int
calculateTotalRisk rm = sum $ (`mod` 3) <$> Arr.elems rm

createErosionMap :: Coord -> Int -> ErosionMap
createErosionMap targetCoord depth = emap
  where
    emap = foldl' f emptyArr $ range bounds
    f arr c = arr Arr.// [(c, gmod (calcGeoIx arr c + depth))]
    calcGeoIx arr c@(V2 x y)
      | c == V2 0 0 = 0
      | c == targetCoord = 0
      | y == 0 = gmod (gmod x * 16807)
      | x == 0 = gmod (gmod y * gmod 48271)
      | otherwise = gmod (gmod (arr Arr.! V2 (x - 1) y) * gmod (arr Arr.! V2 x (y - 1)))
    bounds = (V2 0 0, targetCoord)
    emptyArr = Arr.array bounds []

gmod x = x `mod` 20183

parseInput :: String -> (Int, Coord)
parseInput input = (depth, targetCoord)
  where
    depth                        = read (words depthLine !! 1)
    targetCoord                  = V2 (read x) (read y)
    (x, _ : y)                   = span (/= ',') (words targetLine !! 1)
    (depthLine : targetLine : _) = lines input
