#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Graph.Inductive.Graph (mkGraph, Graph, Node, LEdge, LPath(..))
import Data.Graph.Inductive.Query.SP (spTree)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Ix (range, inRange)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.List (foldl', foldr1, unzip)
import Data.Bifunctor (bimap)

import Debug.Trace (trace)

data Direction = N | E | S | W deriving (Show, Read)
data Stmt = SDir Direction | Options [[Stmt]] deriving (Show)
type Parser = P.Parsec Void String
type G = Gr () Int

main :: IO ()
main = do
    Right stmts <- parseInput . head . lines <$> readFile "day20.input"
    print $ buildGraph stmts

furthestDistance :: G -> Int
furthestDistance g = maximum $ dist . unLPath <$> spTree 0 g
  where
    dist ((_, d):_) = d

buildGraph :: [Stmt] -> G
buildGraph stmts = mkGraph nodes edges
  where
    nodes = (, ()) <$> toList nodeSeq
    edges = toList edgeSeq
    (nodeSeq, edgeSeq) = mkNodesEdges 0 stmts

mkNodesEdges :: Node -> [Stmt] -> (Set Node, Set (LEdge Int))
mkNodesEdges n sss | trace (show (n, length sss)) False = undefined
mkNodesEdges n [] = (Set.empty, Set.empty)
mkNodesEdges n (SDir dir:rest) = (nodes, edges)
  where
    nodes = Set.insert 0 $ Set.insert nextN restNodes
    edges = Set.insert (n, nextN, 1) restEdges
    nextN = next dir n
    (restNodes, restEdges) = mkNodesEdges nextN rest
mkNodesEdges n (Options stmtss:rest) = (nodes, edges)
  where
    (nodes, edges) = bimap Set.unions Set.unions $ unzip $ nodesEdges <$> stmtss
    nodesEdges stmts = mkNodesEdges n (stmts ++ rest)

xk = 10000
mkNode :: Int -> Int -> Int
mkNode x y = x * xk + y
fromNode :: Int -> (Int, Int)
fromNode n = n `quotRem` xk

next :: Direction -> Int -> Int
next N = north
next E = east
next S = south
next W = west

north :: Int -> Int
north n = n - 1
south n = n + 1
east n = n + xk
west n = n - xk

parseInput :: String -> Either (P.ParseError Char Void) [Stmt]
parseInput = P.parse outerParser ""
  where
    outerParser :: Parser [Stmt]
    outerParser = P.between (PC.char '^') (PC.char '$') innerParser
    innerParser :: Parser [Stmt]
    innerParser = P.many stmtParser
    stmtParser :: Parser Stmt
    stmtParser = directionParser P.<|> optionsParser
    optionsParser :: Parser Stmt
    optionsParser = Options <$> parens (innerParser `P.sepBy1` PC.char '|')
    parens = P.between (PC.char '(') (PC.char ')')
    directionParser :: Parser Stmt
    directionParser = toDirection <$> PC.upperChar
    toDirection :: Char -> Stmt
    toDirection c = SDir $ read [c]
