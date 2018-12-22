#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import Data.Graph.Inductive.Graph (mkGraph, Graph, Node, Edge, LPath(..))
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
    edges = (\(a, b) -> (a, b, 1)) <$> toList edgeSeq
    (nodeSeq, edgeSeq) = mkNodesEdges 0 stmts (Set.empty, Set.empty)

mkNodesEdges :: Node -> [Stmt] -> (Set Node, Set Edge) -> (Set Node, Set Edge)
mkNodesEdges n _ (nodes, edges) | trace (show (n, Set.size nodes, Set.size edges)) False = undefined
mkNodesEdges n [] (nodes, edges) = (Set.insert n nodes, edges)
mkNodesEdges n (SDir dir:rest) (!accNodes, !accEdges) = mkNodesEdges nextN rest (nodes, edges)
  where
    nodes = Set.insert n accNodes
    edges = Set.insert (n, nextN) accEdges
    nextN = next dir n
mkNodesEdges n (Options stmtss:rest) (!accNodes, !accEdges) = foldl' f (accNodes, accEdges) stmtss
  where
    f (nodes, edges) stmts = mkNodesEdges n (stmts ++ rest) (nodes, edges)

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
