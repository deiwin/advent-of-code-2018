#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (range, inRange)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.Maybe (maybeToList)
import Data.List (sortOn, groupBy, foldr)

type Coord = V2 Int
type Bounds = (Coord, Coord)
type Vein = (Coord, Coord)
type World = Arr.Array Coord Cell
data Cell = Sand | Clay | StillWater | RunningWater deriving (Show, Eq, Ord)

main :: IO ()
main = do
    Right initialWorld <- parseInput <$> readFile "day17.input"
    let finalWorld = fill initialWorld
    putStr $ pretty finalWorld
    print $ length $ filter isWater $ Arr.elems finalWorld
    print $ length $ filter (== StillWater) $ Arr.elems finalWorld

fill :: World -> World
fill world = fillFrom (V2 500 startY) world
  where
    startY = max 0 minY
    (V2 _ minY) = fst $ Arr.bounds world

fillFrom :: Coord -> World -> World
fillFrom c world
  | isBlockedOrOutside world c = world
  | downBlocked                = rightFilled Arr.// zip (range bounds) (repeat waterType)
  | otherwise                  = downFilled Arr.// [(c, RunningWater)]
  where
    bounds = case (leftBounded, rightBounded) of
        (True , True ) -> (right leftBound, left rightBound)
        (True , False) -> (right leftBound, rightBound)
        (False, True ) -> (leftBound, left rightBound)
        (False, False) -> (leftBound, rightBound)
    waterType                               = if rightBounded && leftBounded then StillWater else RunningWater
    (rightBounded, rightBound, rightFilled) = findBound leftFilled c right
    (leftBounded , leftBound , leftFilled ) = findBound downFilled c left
    downBlocked                             = isBlocked downFilled $ down c
    downFilled                              = fillFrom (down c) world

pretty :: World -> String
pretty world = unlines ls
  where
    ls = fmap char <$> groupBy keySameRow (sortOn keyRow $ Arr.assocs world)
    char (_, StillWater  ) = '~'
    char (_, RunningWater) = '|'
    char (_, Clay        ) = '#'
    char (_, Sand        ) = '.'
    keyRow (V2 _ y, _) = y
    keySameRow a b = keyRow a == keyRow b

findBound :: World -> Coord -> (Coord -> Coord) -> (Bool, Coord, World)
findBound world c inc
  | isBlocked world c = (True, c, world)
  | not (isBlocked world (down c)) = if isWater (world Arr.! down c)
      then (False, c, world)
      else findBound (fillFrom (down c) world) c inc
  | otherwise = findBound world (inc c) inc

left :: Coord -> Coord
left = (+ V2 (-1) 0)
right = (+ V2 1 0)
down = (+ V2 0 1)

isBlockedOrOutside :: World -> Coord -> Bool
isBlockedOrOutside world c = outside || blocked
  where
    outside = not (inRange (Arr.bounds world) c)
    blocked = isBlocked world c

isBlocked :: World -> Coord -> Bool
isBlocked world c | not (inRange (Arr.bounds world) c) = False
                  | Clay <- cell                       = True
                  | StillWater <- cell                 = True
                  | otherwise                          = False
    where cell = world Arr.! c


isWater :: Cell -> Bool
isWater StillWater   = True
isWater RunningWater = True
isWater _            = False

parseInput :: String -> Either P.ParseError World
parseInput input = createWorld <$> veins
  where
    createWorld veins = Arr.accumArray (curry snd) Sand (getBounds veins) (assocs veins)
    assocs veins = do
        vein <- veins
        i <- range vein
        return (i, Clay)
    veins = sequenceA $ parseLine <$> lines input

getBounds :: [Vein] -> Bounds
getBounds veins = (V2 (minimum xs - 1) (minimum ys), V2 (maximum xs + 1) (maximum ys))
  where
    xs = getX <$> coords
    ys = getY <$> coords
    coords = concatMap both veins
    both (a, b) = [a, b]
    getX (V2 x _) = x
    getY (V2 _ y) = y

parseLine :: String -> Either P.ParseError Vein
parseLine = P.parse parser ""
  where
    parser = do
        staticAxis <- P.anyChar
        staticVal <- read <$> (tillEquals *> number)
        (start: end : _) <- fmap read <$> (tillEquals *> numberRange)
        return $ if staticAxis == 'x'
                    then (V2 staticVal start, V2 staticVal end)
                    else (V2 start staticVal, V2 end staticVal)
    number = P.many1 P.digit
    numberRange = number `P.sepBy` P.string ".."
    tillEquals = P.manyTill P.anyChar (P.char '=')
