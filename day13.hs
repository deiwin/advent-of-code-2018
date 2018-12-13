#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Linear.V2 (V2(..))
import Data.Array.IArray (Array, array, bounds, (!), (//), assocs)
import Data.List (cycle, foldl', groupBy, sortOn)
import Text.Show.Functions ()
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

import Debug.Trace (trace)

type State = Map Coord Cart
type TrackMap = Array Coord (Maybe Track)
type Coord = V2 Int
data Track = Vertical | Horizontal | Corner Bool | Intersection deriving (Show)
type Cart = (Direction, Turns)
data Direction = North | East | South | West deriving (Eq, Ord, Enum, Bounded, Show)
type Turns = [Direction -> Direction]

main :: IO ()
main = do
    (trackMap, state) <- parseInput <$> readFile "day13.input"
    print (whileRight (tick trackMap) state)

tick :: TrackMap -> State -> Either Coord State
tick trackMap state | trace (pretty trackMap state) False = undefined
tick trackMap state = if Map.size newState == 1
                         then Left (head (Map.keys newState))
                         else Right newState
  where
    newState = foldl' (update trackMap) state coords
    coords = sortOn rowsFirst $ Map.keys state
    rowsFirst (V2 x y) = (y, x)

pretty :: TrackMap -> State -> String
pretty trackMap state = unlines ls
  where
    ls = fmap toChar <$> groupBy keySameRow (sortOn keyRow $ assocs trackMap)
    toChar (coord, _) | Just North <- direction = '^'
                      | Just East <- direction  = '>'
                      | Just West <- direction  = '<'
                      | Just South <- direction = 'v'
        where direction = fst <$> Map.lookup coord state
    toChar (_, Nothing            ) = ' '
    toChar (_, Just Vertical      ) = '|'
    toChar (_, Just Horizontal    ) = '-'
    toChar (_, Just (Corner False)) = '\\'
    toChar (_, Just (Corner True) ) = '/'
    toChar (_, Just Intersection  ) = '+'
    keyRow (V2 _ y, _) = y
    keySameRow a b = keyRow a == keyRow b

update :: TrackMap -> State -> Coord -> State
update trackMap state coord | Nothing <- Map.lookup coord state = state
update trackMap state coord = nextState
  where
    nextState = foldr ($) state updates
    cart      = state Map.! coord
    nextCoord = case cart of
        (North, _) -> coord + V2 0 (-1)
        (South, _) -> coord + V2 0 1
        (West , _) -> coord + V2 (-1) 0
        (East , _) -> coord + V2 1 0
    apply toState = foldr ($) toState updates
    updates       = case Map.lookup nextCoord state of
        Nothing -> [deleteCurrent, addNew]
        _       -> [deleteCurrent, deleteNew]
    deleteCurrent = Map.delete coord
    deleteNew     = Map.delete nextCoord
    addNew        = Map.insert nextCoord $ case trackMap ! nextCoord of
        Just Intersection  -> turnedCart
        Just (Corner True) -> (, turn : rest) $ case direction of
            North -> East
            East  -> North
            South -> West
            West  -> South
        Just (Corner False) -> (, turn : rest) $ case direction of
            North -> West
            East  -> South
            South -> East
            West  -> North
        Just _ -> cart
    turnedCart               = (turn direction, rest)
    (direction, turn : rest) = cart

parseInput :: String -> (TrackMap, State)
parseInput input = (trackMap, carts)
  where
    trackMap = array bounds (getTrack <$> tracksAndCarts)
    carts    = Map.fromList (catMaybes $ getCart <$> tracksAndCarts)
    bounds   = (V2 0 0, V2 (length (head ls) - 1) (length ls - 1))
    ls       = lines input
    getTrack (coord, (track, _)) = (coord, track)
    getCart (coord, (_, Nothing)) = Nothing
    getCart (coord, (_, Just cart)) = Just (coord, cart)
    tracksAndCarts = do
        (line, y) <- zip (lines input) [0 ..]
        (char, x) <- zip line [0 ..]
        return (V2 x y, parseChar char)
    parseChar ' '  = (Nothing, Nothing)
    parseChar '-'  = (Just Horizontal, Nothing)
    parseChar '|'  = (Just Vertical, Nothing)
    parseChar '\\' = (Just (Corner False), Nothing)
    parseChar '/'  = (Just (Corner True), Nothing)
    parseChar '+'  = (Just Intersection, Nothing)
    parseChar '>'  = (Just Horizontal, Just (East, turns))
    parseChar '<'  = (Just Horizontal, Just (West, turns))
    parseChar '^'  = (Just Vertical, Just (North, turns))
    parseChar 'v'  = (Just Vertical, Just (South, turns))
    turns = cycle [prev, id, next]

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev x = if x == minBound then maxBound else pred x
next :: (Eq a, Enum a, Bounded a) => a -> a
next x = if x == maxBound then minBound else succ x

whileRight :: (a -> Either b a) -> a -> Either b a
whileRight f x | (Right a) <- newX = whileRight f a
               | otherwise         = newX
    where newX = f x
