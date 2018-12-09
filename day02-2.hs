#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (filter, elem, zip, find)

main :: IO ()
main = do
    input <- readFile "day2.input"
    print $ uncurry commonChars <$> pairWithDiffCountOne (lines input)

pairWithDiffCountOne :: [String] -> Maybe (String, String)
pairWithDiffCountOne [] = Nothing
pairWithDiffCountOne (x:xs) =
    case find (\y -> diffCount x y == 1) xs of
      Nothing -> pairWithDiffCountOne xs
      Just y -> Just (x, y)

diffCount :: String -> String -> Int
diffCount as bs = length $ filter (uncurry (/=)) $ zip as bs

commonChars :: String -> String -> String
commonChars as bs = fmap fst $ filter (uncurry (==)) $ zip as bs
