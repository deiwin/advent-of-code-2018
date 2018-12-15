#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Ord (compare, comparing, Ordering)
import Linear.V2 (V2(..))
import Data.Array.IArray (Array, array, bounds, assocs)
import Data.List (foldr, sortOn, groupBy)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

type Coord = V2 Int
type State = (World, Players)
type World = Array Coord IsWall
type Players = Map Coord Player
data Player = Elf Int | Goblin Int deriving (Show, Eq, Ord)
type IsWall = Bool

testInput = unlines [ "#########"
                    , "#G..G..G#"
                    , "#.......#"
                    , "#.......#"
                    , "#G..E..G#"
                    , "#.......#"
                    , "#.......#"
                    , "#G..G..G#"
                    , "#########"
                    ]

main :: IO ()
main = do
    -- input <- parseInput <$> readFile "day15.input"
    let initialState = parseInput testInput
    putStr $ pretty initialState

tick :: State -> State
tick = undefined

pretty :: State -> String
pretty (world, players) = unlines ls
  where
    ls = prettyRow . foldRow <$> groupBy keySameRow (sortOn keyRow $ assocs world)
    prettyRow (s, players) = s <> " " <> show (catMaybes players)
    foldRow = foldr fCell ("", [])
    fCell (coord, isWall) (s, rowPlayers) = let p = Map.lookup coord players in (char isWall p : s, p : rowPlayers)
    char True _                = '#'
    char False (Just (Elf    _)) = 'E'
    char False (Just (Goblin _)) = 'G'
    char False Nothing         = '.'
    keyRow (V2 _ y, _) = y
    keySameRow a b = keyRow a == keyRow b

targetOrder :: (Coord, Player) -> (Coord, Player) -> Ordering
targetOrder = comparing snd <> comparing ((\(V2 x y) -> (y, x)) . fst)
readingOrder :: Coord -> Coord -> Ordering
readingOrder = comparing (\(V2 x y) -> (y, x))

isAlly :: Player -> Player -> Bool
isAlly a b = isElf a == isElf b
isElf :: Player -> Bool
isElf (Elf _) = True
isElf _       = False
isGoblin :: Player -> Bool
isGoblin = not . isElf

parseInput :: String -> State
parseInput input = (world, players)
  where
    world   = array bounds $ getCell <$> assocs
    players = Map.fromList (catMaybes $ getPlayer <$> assocs)
    bounds  = (V2 0 0, V2 (length (head ls) - 1) (length ls - 1))
    ls      = lines input
    assocs  = do
        (line, y) <- zip (lines input) [0 ..]
        (char, x) <- zip line [0 ..]
        return (V2 x y, parseChar char)
    getCell (coord, (cell, _)) = (coord, cell)
    getPlayer (coord, (_, Nothing)    ) = Nothing
    getPlayer (coord, (_, Just player)) = Just (coord, player)
    parseChar '#' = (True, Nothing)
    parseChar '.' = (False, Nothing)
    parseChar 'E' = (False, Just (Elf 200))
    parseChar 'G' = (False, Just (Goblin 200))
