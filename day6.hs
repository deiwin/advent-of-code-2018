#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package linear
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, sortOn)
import Linear.V2 (V2(..))
import Data.Ix (range, inRange)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Control.Applicative (empty)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type Counts = Map Coord Int

main :: IO ()
main = do
    input <- lines <$> readFile "day6.input"
    let coords = toV2 . fmap read . words . filter (/= ',') <$> input
    let bounds = getBounds coords
    let sideIDs = getSideIDs bounds coords
    print $ maximum $ Map.elems $ Map.filterWithKey (notIn sideIDs) $ dangerousCells bounds coords
  where
    notIn ids id _ = id `notElem` ids

dangerousCells :: Bounds -> [Coord] -> Counts
dangerousCells bounds coords = Map.fromListWith (+) $ do
    coord <- range bounds
    case closest coords coord of
      Nothing -> empty
      Just c -> return (c, 1)

getSideIDs :: Bounds -> [Coord] -> [Coord]
getSideIDs bounds coords = nub $ do
    coord <- range bounds
    guard $ isSide coord
    case closest coords coord of
      Nothing -> empty
      Just c -> return c
  where
    isSide coord = any (not . inRange bounds) $ surroundingCoords coord

closest :: [Coord] -> Coord -> Maybe Coord
closest coords coord =
    let ((xDist, x):(yDist, y):_) = sortOn fst $ zip (dist coord <$> coords) coords
     in if xDist == yDist
           then Nothing
           else Just x

dist :: Coord -> Coord -> Int
dist a b = vsum (a - b)
    where vsum (V2 x y) = abs x + abs y

surroundingCoords :: Coord -> [Coord]
surroundingCoords coord = (coord +) . uncurry V2 <$> [(-1, 0), (0, -1), (1, 0), (0, 1)]

getBounds :: [Coord] -> Bounds
getBounds coords = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    xs = getX <$> coords
    ys = getY <$> coords
    getX (V2 x _) = x
    getY (V2 _ y) = y

toV2 :: [Int] -> Coord
toV2 (x:y:_) = V2 x y
