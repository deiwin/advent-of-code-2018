#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package sbv
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Linear.V3 (V3(..))
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.SBV (optimize, OptimizeStyle(Lexicographic), maximize, minimize, sInteger, SInteger
  , literal, SBV, (.<=), oneIf)

type Point = V3 Int
type Bot = (Point, Int)

main :: IO ()
main = do
    Right bots <- parseInput <$> readFile "day23.input"
    print $ inRangeOfStrongest bots
    result <- bestPointDist bots
    print result

bestPointDist :: [Bot] -> _
bestPointDist bots = optimize Lexicographic $ do
    x <- sInteger "x"
    y <- sInteger "y"
    z <- sInteger "z"
    let p = V3 x y z
    maximize "inRangeCount" $ countInRange p
    minimize "distance" $ mDist' p start
  where
    start = V3 0 0 0
    inRangeOf p (p', r) = mDist' p p' .<= toLit r
    countInRange :: _ -> SInteger
    countInRange p = sum (oneIf . (p `inRangeOf`) <$> bots)
    mDist' :: _ -> Point -> SBV Integer
    mDist' (V3 x y z) (V3 x' y' z') = abs (x - toLit x') + abs (y - toLit y') + abs (z - toLit z')
    toLit :: Int -> SInteger
    toLit x = literal $ fromIntegral x

inRangeOfStrongest :: [Bot] -> Int
inRangeOfStrongest bots = length $ filter (inRangeOf strongestBot) bots
  where
    strongestBot = maximumBy (comparing snd) bots
    inRangeOf (pa, r) (pb, _) = mDist pa pb <= r

mDist :: Point -> Point -> Int
mDist a b = sum $ abs (a - b)

parseInput :: String -> Either P.ParseError [Bot]
parseInput input = sequenceA $ parseLine <$> lines input

parseLine :: String -> Either P.ParseError Bot
parseLine = P.parse parser ""
  where
    parser = do
        [x, y, z] <- fmap read <$> (till '<' *> commaSep signedNum)
        r <- read <$> (till '=' *> num)
        return (V3 x y z, r)
    signedNum = P.many1 (P.char '-' <|> P.digit)
    num = P.many1 P.digit
    commaSep p = p `P.sepBy` P.char ','
    till c = P.manyTill P.anyChar (P.char c)
