#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Ord (compare, comparing, Ordering)
import Linear.V2 (V2(..))
import qualified Data.Array.IArray as Arr
import Data.Ix (range)
import Data.List (foldr, sortOn, groupBy, minimumBy, sortBy)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe, isNothing, listToMaybe)
import Data.Foldable (foldlM)
import Data.Either (either, isRight)
import Data.Graph.Inductive.Graph (mkGraph, Graph)
import Data.Graph.Inductive.Query.SP (spTree)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Internal.RootPath (getDistance)

import Debug.Trace (trace)

type Coord = V2 Int
type World = Arr.Array Coord IsWall
type Players = Map Coord Player
data Player = Elf Int Int | Goblin Int Int deriving (Show, Eq, Ord)
type IsWall = Bool
type Path = [Coord]

main :: IO ()
main = do
    (world, players) <- parseInput <$> readFile "day15.input"
    print $ uncurry outcome $ finishCombat 3 world players
    print $ uncurry outcome $ finishCombatWithoutDeaths world players

outcome :: Int -> Players -> Int
outcome rounds players = rounds * sum (hp <$> Map.elems players)

finishCombatWithoutDeaths :: World -> Players -> (Int, Players)
finishCombatWithoutDeaths world players = head $ dropWhile anElfDied results
  where
    anElfDied (_, players') = elfCount players' < initialElfCount
    results = (\x -> finishCombat x world players) <$> [4..]
    initialElfCount = elfCount players
    elfCount m = Map.size $ Map.filter isElf m

finishCombat :: Int -> World -> Players -> (Int, Players)
finishCombat power world players = (i, finalPlayers)
  where
      (i, Left finalPlayers) = head
        $ dropWhile (isRight . snd)
        $ zip [(-1)..]
        $ iterate (>>= tick world power) (return players)

tick :: World -> Int -> Players -> Either Players Players
tick world power players | trace (pretty world players) False = undefined
tick world power players = foldlM (takeTurn world power) players playersInOrder
  where
    playersInOrder = sortOn (readingOrder . fst) $ Map.assocs players

takeTurn :: World -> Int -> Players -> (Coord, Player) -> Either Players Players
takeTurn world power players (c, p)
  | Nothing <- updatedPlayer = Right players -- Skip if dead
  | Just p' <- updatedPlayer
  , playerID p' /= playerID p = Right players -- Skip if other player in place
  | Map.size (Map.filter (areEnemies p) players) == 0 = Left players -- End if over
  | Just p' <- updatedPlayer = Right (newPlayers p')
  where
    newPlayers p' = snd $ uncurry (attack world power) $ move world (c, p') players
    updatedPlayer = players Map.!? c

move :: World -> (Coord, Player) -> Players -> ((Coord, Player), Players)
move world p players | enemyInRange || null reachableDestinations = (p, players)
                     | otherwise = (newP, newPlayers)
  where
    enemyInRange = any (areEnemies $ snd p) $ catMaybes $ (players Map.!?) <$> adjacencies (fst p)
    newPlayers   = uncurry Map.insert newP $ Map.delete (fst p) players
    newP         = (step, snd p)
    step :: Coord
    step                = fst $ snd chosenDestination
    chosenDestination   = minimumBy (comparing (readingOrder . fst)) closestDestinations
    closestDestinations = filterMinimumOn dist reachableDestinations
    reachableDestinations :: [(Coord, (Coord, Int))]
    reachableDestinations = catMaybes bestStepsToDestination
    dist :: (Coord, (Coord, Int)) -> Int
    dist (_, (_, d)) = d
    bestStepsToDestination = (\c -> (c, ) <$> bestStep world players gr (fst p) c) <$> destinations
    gr = graph world players
    destinations            = filter (isEmpty world players) enemyAdjacencies
    enemyAdjacencies :: [Coord]
    enemyAdjacencies = concatMap adjacencies $ Map.keys enemies
    enemies          = Map.filter (areEnemies $ snd p) players

adjacencies :: Coord -> [Coord]
adjacencies c = (+ c) <$> [V2 0 (-1), V2 (-1) 0, V2 1 0, V2 0 1]

isEmpty :: World -> Players -> Coord -> Bool
isEmpty world players c = not (world Arr.! c) && isNothing (players Map.!? c)

filterMinimumOn :: Ord b => (a -> b) -> [a] -> [a]
filterMinimumOn f xs = filter ((== g) . f) xs where g = minimum $ f <$> xs

bestStep :: World -> Players -> Gr Coord Int -> Coord -> Coord -> Maybe (Coord, Int)
bestStep world players gr from to = step
  where
    step = if null shortestSteps
              then Nothing
              else listToMaybe $ sortBy (comparing snd <> comparing (readingOrder . fst)) shortestSteps
    shortestSteps = mapMaybe shortestStep $ filter (isEmpty world players) $ adjacencies from
    shortestStep x = (x, ) <$> getDistance (mkNode x) tree
    tree = spTree (mkNode to) gr
    mkNode :: Coord -> Int
    mkNode (V2 x y) = x + 1000 * y

graph :: World -> Players -> Gr Coord Int
graph world players = mkGraph (mkNode <$> nodes) (mkEdge <$> edges)
  where
    nodes :: [Coord]
    nodes = filter (isEmpty world players) $ range $ Arr.bounds world
    edges :: [(Coord, Coord)]
    edges = concatMap (\c -> fmap (c, ) $ filter (isEmpty world players) $ adjacencies c) nodes
    mkNode :: Coord -> (Int, Coord)
    mkNode (V2 x y) = (x + 1000 * y, V2 x y)
    mkEdge :: (Coord, Coord) -> (Int, Int, Int)
    mkEdge (a, b) = (fst $ mkNode a, fst $ mkNode b, 1)
    unNode :: Int -> Coord
    unNode i = uncurry (flip V2) $ divMod i 1000

attack :: World -> Int -> (Coord, Player) -> Players -> ((Coord, Player), Players)
attack world power p players = case chosenEnemy of
                           Nothing -> (p, players)
                           Just enemy -> (p, newPlayers enemy)
  where
    newPlayers attackedEnemy = let newEnemy = damage attackedEnemy
                                in if hp (snd newEnemy) <= 0
                                      then Map.delete (fst attackedEnemy) players
                                      else uncurry Map.insert newEnemy players
    updatedEnemy = damage <$> chosenEnemy
    damage (c, Elf id hp) = (c, Elf id $ hp - 3)
    damage (c, Goblin id hp) = (c, Goblin id $ hp - power)
    chosenEnemy = listToMaybe $ sortBy enemyOrder enemiesInRange
    enemyOrder = comparing (hp . snd) <> comparing (readingOrder . fst)
    enemiesInRange = filter (areEnemies (snd p) . snd) $ catMaybes $ getPlayer <$> adjacencies (fst p)
    getPlayer c = (c, ) <$> players Map.!? c

hp :: Player -> Int
hp (Elf    _ x) = x
hp (Goblin _ x) = x
playerID :: Player -> Int
playerID (Elf    x _) = x
playerID (Goblin x _) = x

putPretty :: World -> Players -> IO ()
putPretty w p = putStr $ pretty w p

pretty :: World -> Players -> String
pretty world players = unlines ls
  where
    ls = prettyRow . foldRow <$> groupBy keySameRow (sortOn keyRow $ Arr.assocs world)
    prettyRow (s, players) = s <> " " <> show (catMaybes players)
    foldRow = foldr fCell ("", [])
    fCell (coord, isWall) (s, rowPlayers) = let p = Map.lookup coord players in (char isWall p : s, p : rowPlayers)
    char True  _                   = '#'
    char False (Just (Elf    _ _)) = 'E'
    char False (Just (Goblin _ _)) = 'G'
    char False Nothing             = '.'
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
isElf (Elf _ _) = True
isElf _         = False
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
        let id = x + 1000 * y
        return (V2 x y, parseChar id char)
    getCell (coord, (cell, _)) = (coord, cell)
    getPlayer (coord, (_, Nothing)    ) = Nothing
    getPlayer (coord, (_, Just player)) = Just (coord, player)
    parseChar _  '#' = (True, Nothing)
    parseChar _  '.' = (False, Nothing)
    parseChar id 'E' = (False, Just (Elf id 200))
    parseChar id 'G' = (False, Just (Goblin id 200))
