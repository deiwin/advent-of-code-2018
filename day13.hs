#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Linear.V2 (V2(..))
import Data.Array.IArray (Array, array, bounds, (!), (//), assocs)
import Data.List (cycle, foldl', groupBy, sortOn)
import Text.Show.Functions ()

type State = Array Coord Cell
type Coord = V2 Int
type Bounds = (Coord, Coord)
type Cell = Either (Maybe Track) (Track, Cart)
data Track = Vertical | Horizontal | Corner Bool | Intersection deriving (Show)
type Cart = (Direction, Turns)
data Direction = North | East | South | West deriving (Eq, Ord, Enum, Bounded, Show)
type Turns = [Direction -> Direction]

main :: IO ()
main = do
    state <- parseInput <$> readFile "day13.input"
    print (whileRight tick state)

tick :: State -> Either Coord State
tick state = foldl' update (Right state) $ do
    let (V2 x1 y1, V2 x2 y2) = bounds state
    y <- [y1..y2]
    x <- [x1..x2]
    return (V2 x y)

update :: Either Coord State -> Coord -> Either Coord State
update (Left  coord) _     = Left coord
update (Right state) coord = f (state ! coord)
  where
    f (Left _) = Right state
    f (Right (track, cart)) =
        (state //) . (: [(coord, trackCell track)]) <$> moveUpdate cart (nextCell state cart coord)
    trackCell track = Left (Just track)

moveUpdate :: Cart -> (Cell, Coord) -> Either Coord (Coord, Cell)
moveUpdate cart (Right _                  , coord) = Left coord
moveUpdate cart (Left  (Just Intersection), coord) = Right (coord, newCell)
  where
    newCell                  = Right (Intersection, newCart)
    newCart                  = (turn direction, rest)
    (direction, turn : rest) = cart
moveUpdate cart (Left (Just (Corner forwardSlash)), coord) = Right (coord, newCell)
  where
    newCell      = Right (Corner forwardSlash, newCart)
    newCart      = (newDirection, turns)
    newDirection = case (direction, forwardSlash) of
        (North, True ) -> East
        (East , True ) -> North
        (South, True ) -> West
        (West , True ) -> South
        (North, False) -> West
        (East , False) -> South
        (South, False) -> East
        (West , False) -> North
    (direction, turns) = cart
moveUpdate cart (Left (Just track), coord) = Right (coord, Right (track, cart))

nextCell :: State -> Cart -> Coord -> (Cell, Coord)
nextCell state cart coord = (state ! nextCoord, nextCoord)
  where
    nextCoord = case cart of
        (North, _) -> coord + V2 0 (-1)
        (South, _) -> coord + V2 0 1
        (West , _) -> coord + V2 (-1) 0
        (East , _) -> coord + V2 1 0

parseInput :: String -> State
parseInput input = array bounds assocs
  where
    bounds = (V2 0 0, V2 (length (head ls) - 1) (length ls - 1))
    ls     = lines input
    assocs = do
        (line, y) <- zip (lines input) [0 ..]
        (char, x) <- zip line [0 ..]
        return (V2 x y, parseChar char)
    parseChar ' '  = Left Nothing
    parseChar '-'  = Left (Just Horizontal)
    parseChar '|'  = Left (Just Vertical)
    parseChar '\\' = Left (Just (Corner False))
    parseChar '/'  = Left (Just (Corner True))
    parseChar '+'  = Left (Just Intersection)
    parseChar '>'  = Right (Horizontal, (East, turns))
    parseChar '<'  = Right (Horizontal, (West, turns))
    parseChar '^'  = Right (Vertical, (North, turns))
    parseChar 'v'  = Right (Vertical, (South, turns))
    turns = cycle [prev, id, next]

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev x = if x == minBound then maxBound else pred x
next :: (Eq a, Enum a, Bounded a) => a -> a
next x = if x == maxBound then minBound else succ x

whileRight :: (a -> Either b a) -> a -> Either b a
whileRight f x | (Right a) <- newX = whileRight f a
               | otherwise         = newX
    where newX = f x
