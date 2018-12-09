#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Graph (Forest, Tree(..))
import Data.List (foldl', (!!))
import Data.Foldable (foldMap)
import Data.Monoid (Sum(..))

main :: IO ()
main = do
    input <- words . head . lines <$> readFile "day8.input"
    let (root, _) = readTree $ read <$> input
    print $ getSum $ foldMap (Sum . sum) root
    print $ treeVal root

treeVal :: Tree [Int] -> Int
treeVal (Node metadata []) = sum metadata
treeVal (Node metadata ts) = sum childrenVals
  where
    childrenVals = treeVal . (ts !!) <$> safeIndices
    safeIndices  = filter isSafeIndex $ (+ (-1)) <$> metadata
    isSafeIndex i = i >= 0 && i < length ts

readTree :: [Int] -> (Tree [Int], [Int])
readTree (children : metaCount : rest) = (Node metadata ts, remainder)
    where (ts, metadata, remainder) = readChildren children metaCount rest []

readChildren :: Int -> Int -> [Int] -> [Tree [Int]] -> ([Tree [Int]], [Int], [Int])
readChildren 0        metaCount input ts = (reverse ts, take metaCount input, drop metaCount input)
readChildren children metaCount input ts = readChildren (children - 1) metaCount remainder (t : ts)
    where (t, remainder) = readTree input
