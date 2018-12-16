#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Ord (compare, comparing, Ordering)
import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (range)
import Data.List (foldr, sortOn, groupBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Foldable (foldlM)
import Data.Either (either)
import Data.Graph.Inductive.Graph (mkGraph, Graph, LPath(..))
import Data.Graph.Inductive.Query.SP (spTree, LRTree)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Internal.RootPath (getDistance)

import Debug.Trace (trace)

type Coord = V2 Int
type World = Arr.Array Coord IsWall
type Players = Map Coord Player
data Player = Elf Int | Goblin Int deriving (Show, Eq, Ord)
type IsWall = Bool
type Path = [Coord]

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
    let (world, players) = parseInput testInput
    print $ bestPaths world players (V2 1 1) (V2 2 2)
    either (putPretty world) (putPretty world) $ tick world players
    either (putPretty world) (putPretty world) $ tick world =<< tick world players
    either (putPretty world) (putPretty world) $ tick world =<< tick world =<< tick world players
    either (putPretty world) (putPretty world) $ tick world =<< tick world =<< tick world =<< tick world players
    either (putPretty world) (putPretty world) $ tick world =<< tick world =<< tick world =<< tick world =<< tick world players

tick :: World -> Players -> Either Players Players
tick world players = foldlM (takeTurn world) players playersInOrder
  where
    playersInOrder = sortOn (readingOrder . fst) $ Map.assocs players

takeTurn :: World -> Players -> (Coord, Player) -> Either Players Players
takeTurn world players p
  | Map.size (Map.filter (areEnemies $ snd p) players) == 0 = Left players
  | otherwise = Right newPlayers
  where
    newPlayers = snd $ uncurry (attack world) $ move world p players

move :: World -> (Coord, Player) -> Players -> ((Coord, Player), Players)
move world p players | enemyInRange || null reachableDestinations = (p, players)
                     | otherwise = (newP, newPlayers)
  where
    enemyInRange = any (areEnemies $ snd p) $ catMaybes $ (players Map.!?) <$> adjacencies (fst p)
    newPlayers   = uncurry Map.insert newP $ Map.delete (fst p) players
    newP         = (step, snd p)
    step :: Coord
    step                = minimumBy (comparing readingOrder) potentialFirstSteps
    potentialFirstSteps = head <$> snd chosenDestination
    chosenDestination   = minimumBy (comparing (readingOrder . fst)) closestDestinations
    closestDestinations =
        let d = minimum (dist <$> reachableDestinations) in filter ((== d) . dist) bestPathsToDestinations
    reachableDestinations :: [(Coord, [Path])]
    reachableDestinations = filter (not . null . snd) bestPathsToDestinations
    dist :: (Coord, [Path]) -> Int
    dist (_, paths) = length $ head paths
    bestPathsToDestinations = zip destinations (bestPaths world players (fst p) <$> destinations)
    destinations            = filter (isEmpty world players) enemyAdjacencies
    enemyAdjacencies :: [Coord]
    enemyAdjacencies = concatMap adjacencies $ Map.keys enemies
    enemies          = Map.filter (areEnemies $ snd p) players

adjacencies :: Coord -> [Coord]
adjacencies c = (+ c) <$> [V2 0 (-1), V2 (-1) 0, V2 1 0, V2 0 1]

isEmpty :: World -> Players -> Coord -> Bool
isEmpty world players c = not (world Arr.! c) && isNothing (players Map.!? c)

bestPaths :: World -> Players -> Coord -> Coord -> [Path]
bestPaths world players from to = paths
  where
    paths = case distance of
              Nothing -> []
              Just d -> drop 1 . reverse . fmap unNode . unLPath <$> filter (best d) shortestPaths
    best d (LP []) = False
    best d (LP ((w, d'):_)) = w == fst (mkNode to) && d == d'
    distance = getDistance (fst $ mkNode to) shortestPaths
    shortestPaths :: LRTree Int
    shortestPaths = spTree (fst $ mkNode from) graph
    graph :: Gr Coord Int
    graph = mkGraph (mkNode <$> nodes) (mkEdge <$> edges)
    nodes :: [Coord]
    nodes = filter included $ range $ Arr.bounds world
    edges :: [(Coord, Coord)]
    edges = concatMap (\c -> fmap (c, ) $ filter included $ adjacencies c) nodes
    included c = c == from || c == to || isEmpty world players c
    mkNode :: Coord -> (Int, Coord)
    mkNode (V2 x y) = (x + 1000 * y, V2 x y)
    mkEdge :: (Coord, Coord) -> (Int, Int, Int)
    mkEdge (a, b) = (fst $ mkNode a, fst $ mkNode b, 1)
    unNode :: (Int, Int) -> Coord
    unNode (i, _) = uncurry (flip V2) $ divMod i 1000

attack :: World -> (Coord, Player) -> Players -> ((Coord, Player), Players)
attack world p players = (p, players)

putPretty :: World -> Players -> IO ()
putPretty w p = putStr $ pretty w p

pretty :: World -> Players -> String
pretty world players = unlines ls
  where
    ls = prettyRow . foldRow <$> groupBy keySameRow (sortOn keyRow $ Arr.assocs world)
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
targetOrder = comparing snd <> comparing (readingOrder . fst)
readingOrder :: Coord -> (Int, Int)
readingOrder (V2 x y) = (y, x)

areEnemies :: Player -> Player -> Bool
areEnemies a b = not $ areAllies a b
areAllies :: Player -> Player -> Bool
areAllies a b = isElf a == isElf b
isElf :: Player -> Bool
isElf (Elf _) = True
isElf _       = False
isGoblin :: Player -> Bool
isGoblin = not . isElf

parseInput :: String -> (World, Players)
parseInput input = (world, players)
  where
    world   = Arr.array bounds $ getCell <$> assocs
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
