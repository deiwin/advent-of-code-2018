#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, lines)
import Data.Text (lines)
import Data.Text.IO (readFile)
import Data.Text.Read (signed, decimal)
import Data.Traversable (sequenceA)
import Data.List (cycle)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    input <- readFile "day1.input"
    let Right changes = fmap fst <$> sequenceA (signed decimal <$> lines input)
    print $ findRepeatingFrequency (cycle changes) Set.empty 0

findRepeatingFrequency :: [Integer] -> Set Integer -> Integer -> Integer
findRepeatingFrequency (change : rest) seenFrequencies currentFrequency =
    if Set.member newFrequency seenFrequencies
        then newFrequency
        else findRepeatingFrequency rest newSeenFrequencies newFrequency
  where
    newFrequency       = currentFrequency + change
    newSeenFrequencies = Set.insert newFrequency seenFrequencies
