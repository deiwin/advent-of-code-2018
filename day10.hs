#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Linear.V2 (V2(..))
import Data.List (iterate, repeat, sortOn, groupBy, find, dropWhile)
import Data.Array.IArray (Array, accumArray, assocs)
import Data.Foldable (traverse_)
import Control.Applicative (empty)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type Grid = Array Coord Char
type Velocity = V2 Int

main :: IO ()
main = do
    input <- lines <$> readFile "day10.input"
    let Right stars = sequenceA $ parseLine <$> input
    let results = take 10 $ dropWhile (not . smallRowRange . fst) $ take 100000 $ zip (iterate tick stars) [0 ..]
    traverse_ p results
  where
    p (stars, i) = do
        print i
        prettyPrint $ fst <$> stars

smallRowRange :: [(Coord, Velocity)] -> Bool
smallRowRange stars = isSmall bounds
  where
    isSmall (V2 _ a, V2 _ b) = abs (a - b) < 20
    bounds = getBounds $ fst <$> stars

tick :: [(Coord, Velocity)] -> [(Coord, Velocity)]
tick = fmap add
    where add (c, v) = (c + v, v)

prettyPrint :: [Coord] -> IO ()
prettyPrint cs = do
    let grid = buildGrid cs
    traverse_ putStrLn lines
  where
    lines = fmap snd <$> groupBy keySameRow (sortOn keyRow $ assocs grid)
    grid = buildGrid cs
    keyRow (V2 _ y, _) = y
    keySameRow a b = keyRow a == keyRow b

buildGrid :: [Coord] -> Grid
buildGrid cs = accumArray (curry snd) '.' b assocs
  where
    b = getBounds cs
    assocs = zip cs (repeat '#')

getBounds :: [Coord] -> Bounds
getBounds coords = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    xs = getX <$> coords
    ys = getY <$> coords
    getX (V2 x _) = x
    getY (V2 _ y) = y

parseLine :: String -> Either P.ParseError (Coord, Velocity)
parseLine = P.parse parser ""
  where
    parser = do
        (x  : y  : _) <- xyParser
        (vx : vy : _) <- xyParser
        return (V2 x y, V2 vx vy)
    xyParser  = fmap read <$> (P.manyTill P.anyChar (P.char '<') *> commaSep (P.many P.space *> signedNum))
    signedNum = P.many1 (P.char '-' <|> P.digit)
    commaSep p = p `P.sepBy` P.char ','
