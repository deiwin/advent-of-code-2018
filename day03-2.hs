#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (foldl1', any, tails)
import Data.Array.IArray (Array, accumArray, elems)
import Data.Ix (range, inRange)
import qualified Text.Parsec as P

type Matrix = Array Index Int
type Index = (Int, Int)
type Bounds = (Index, Index)

main :: IO ()
main = do
    input <- readFile "day3.input"
    let Right claims = sequenceA $ parseLine <$> lines input
    print $ findNonOverlapping claims claims

findNonOverlapping :: [(Int, Bounds)] -> [(Int, Bounds)] -> Int
findNonOverlapping ((id, bounds) : xs) allClaims
  | any (overlap bounds) allOtherBounds = findNonOverlapping xs allClaims
  | otherwise = id
  where
    allOtherBounds = snd <$> withoutCurrent allClaims
    withoutCurrent = filter (\x -> fst x /= id)

overlap :: Bounds -> Bounds -> Bool
overlap a b = a `hasCornerIn` b || b `hasCornerIn` a
  where
    hasCornerIn a b = any (inRange b) $ corners a
    corners ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

parseLine :: String -> Either P.ParseError (Int, Bounds)
parseLine line = do
    parsedNumbers <- fmap read <$> P.parse parser "" line
    let id     = head parsedNumbers
    let x      = parsedNumbers !! 1
    let y      = parsedNumbers !! 2
    let width  = parsedNumbers !! 3
    let height = parsedNumbers !! 4
    return (id, ((x, y), (x + width - 1, y + height - 1)))
  where
    parser =
        P.char '#'
            *> P.many1 (P.many1 P.digit)
            <> (P.manyTill P.anyChar (P.char '@') *> spaces *> commaSep (P.many1 P.digit))
            <> (P.string ": " *> xSep (P.many1 P.digit))
    commaSep p = p `P.sepBy` P.char ','
    xSep p = p `P.sepBy` P.char 'x'
    spaces = P.skipMany1 P.space
