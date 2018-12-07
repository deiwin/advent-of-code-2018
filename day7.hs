#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord, chr)
import Data.List (reverse, notElem)
import Data.Graph (Graph, Edge, Vertex, buildG, indegree, edges)
import Data.Array.IArray (elems, assocs, bounds, array)

main :: IO ()
main = do
    input <- lines <$> readFile "day7.input"
    let graph = buildGraph $ parseLine <$> input
    print $ getOrder graph

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

buildGraph :: [Edge] -> Graph
buildGraph = buildG (ord 'A', ord 'Z')

parseLine :: String -> Edge
parseLine s = (from, to)
  where
    (from:to:_) = ords
    ords = ord . head <$> [ws !! 1, ws !! 7]
    ws = words s
