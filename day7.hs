#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord, chr)
import Data.List (reverse, notElem, partition, sort, repeat)
import Data.Graph (Graph, Edge, Vertex, buildG, indegree, edges)
import Data.Array.IArray (elems, assocs, bounds, array)

main :: IO ()
main = do
    input <- lines <$> readFile "day7.input"
    let graph = buildGraph $ parseLine <$> input
    print $ getOrder graph
    print $ parallelAlphaTopSort graph

getOrder :: Graph -> String
getOrder g = chr <$> alphaTopSort g

alphaTopSort :: Graph -> [Vertex]
alphaTopSort g = reverse $ alphaTopSort' g []
alphaTopSort' :: Graph -> [Vertex] -> [Vertex]
alphaTopSort' g acc
  | length (elems g) == length acc = acc
  | otherwise = alphaTopSort' newG newAcc
  where
    newG = array (bounds g) newAssocs
    newAssocs = (removed, []) : filter ((/= removed) . fst) (assocs g)
    newAcc = removed:acc
    removed = minimum $ fmap fst $ filter removable $ assocs $ indegree g
    removable x = noIncoming x && fst x `notElem` acc
    noIncoming (_, 0) = True
    noIncoming _      = False

parallelAlphaTopSort :: Graph -> Int
parallelAlphaTopSort g = parallelAlphaTopSort' g 0 [] []
parallelAlphaTopSort' :: Graph -> Int -> [(Vertex, Int)] -> [Vertex] -> Int
parallelAlphaTopSort' g elapsedTime wip acc
  | length (elems g) == length acc && null wip = elapsedTime
  | null wip = parallelAlphaTopSort' newG elapsedTime nextWip newAcc
  | otherwise = parallelAlphaTopSort' newG nextTime nextWip newAcc
  where
    newG = array (bounds g) newAssocs
    newAssocs = zip (fst <$> finishedWork) (repeat []) ++ filter ((`notElem` (fst <$> finishedWork)) . fst) (assocs g)
    (finishedWork, inProgressWork) = partition ((<= 0) . snd) (doWork <$> wip)
    doWork (v, x) = (v, x - 1)
    nextTime = elapsedTime + 1
    nextWip = inProgressWork ++ zip newWork (workTime <$> newWork)
    newAcc = newWork ++ acc
    newWork = take freeWorkerCount allRemovable
    freeWorkerCount = 5 - length inProgressWork
    allRemovable = sort $ fmap fst $ filter removable $ assocs $ indegree newG
    removable x = noIncoming x && fst x `notElem` acc
    noIncoming (_, 0) = True
    noIncoming _      = False
    workTime v = v - 4

buildGraph :: [Edge] -> Graph
buildGraph = buildG (ord 'A', ord 'Z')

parseLine :: String -> Edge
parseLine s = (from, to)
  where
    (from:to:_) = ords
    ords = ord . head <$> [ws !! 1, ws !! 7]
    ws = words s
