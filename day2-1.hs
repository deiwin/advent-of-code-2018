#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort, group, filter, elem)

main :: IO ()
main = do
    input <- readFile "day2.input"
    let letterLenghts = fmap length . group . sort <$> lines input
    let twoCounts = length $ filter (elem 2) letterLenghts
    let threeCounts = length $ filter (elem 3) letterLenghts
    print $ twoCounts * threeCounts
