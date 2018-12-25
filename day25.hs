#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package astar
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V4 (V4(..))
import Data.List (foldl', partition)

type Point = V4 Int
type Constellation = Set Point

main :: IO ()
main = do
    points <- parseInput <$> TIO.readFile "day25.input"
    print $ length $ buildConstellations points

buildConstellations :: [Point] -> [Constellation]
buildConstellations = foldl' f []
  where
    f cs p = let (toMerge, rest) = partition (S.member p) cs
              in S.unions (singleton p : toMerge) : rest

singleton :: Point -> Constellation
singleton p@(V4 x y z w) = S.fromList $ filter ((<= 3) . mDist p) $ do
    x' <- close x
    y' <- close y
    z' <- close z
    w' <- close w
    return (V4 x' y' z' w')
  where close x = [(x-3)..(x+3)]

mDist :: Point -> Point -> Int
mDist a b = sum $ abs (a - b)

parseInput :: T.Text -> [Point]
parseInput t = parseLine <$> T.lines t
  where
    parseLine :: T.Text -> Point
    parseLine l = let Right points = sequenceA $ TR.signed TR.decimal <$> T.splitOn "," l
                   in toPoint $ fst <$> points
    toPoint (a:b:c:d:_) = V4 a b c d
