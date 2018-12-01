#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, lines)
import Data.Text (lines)
import Data.Text.IO (readFile)
import Data.Text.Read (signed, decimal)
import Data.Traversable (sequenceA)

main = do
    input <- readFile "day1-1.input"
    let result = sum . fmap fst <$> sequenceA (signed decimal <$> lines input)
    print result
