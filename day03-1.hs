#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl1')
import Data.Array.IArray (Array, accumArray, elems)
import Data.Ix (range)
import qualified Text.Parsec as P

type Matrix = Array Index Int
type Index = (Int, Int)
type Bounds = (Index, Index)

main :: IO ()
main = do
    input <- readFile "day3.input"
    let Right claims  = sequenceA $ parseLine <$> lines input
    let bounds        = foldl1' combineBounds claims
    let arr :: Matrix = accumArray (+) 0 bounds $ concatMap claimAssocList claims
    print $ length $ filter (> 1) $ elems arr

claimAssocList :: Bounds -> [(Index, Int)]
claimAssocList claimBounds = do
    i <- range claimBounds
    return (i, 1)

combineBounds :: Bounds -> Bounds -> Bounds
combineBounds a b = ((min ax1 bx1, min ay1 by1), (max ax2 bx2, max ay2 by2))
  where
    ((ax1, ay1), (ax2, ay2)) = a
    ((bx1, by1), (bx2, by2)) = b

parseLine :: String -> Either P.ParseError Bounds
parseLine line = do
    parsedNumbers <- fmap read <$> P.parse parser "" line
    let x      = head parsedNumbers
    let y      = parsedNumbers !! 1
    let width  = parsedNumbers !! 2
    let height = parsedNumbers !! 3
    return ((x, y), (x + width - 1, y + height - 1))
  where
    parser =
        P.manyTill P.anyChar (P.char '@')
            *> spaces
            *> commaSep (P.many1 P.digit)
            <> (P.string ": " *> xSep (P.many1 P.digit))
    commaSep p = p `P.sepBy` P.char ','
    xSep p = p `P.sepBy` P.char 'x'
    spaces = P.skipMany1 P.space
