#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Text.Parsec as P
import Data.List (sort, foldl', maximumBy)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map

type ID = Int
type Minute = Int
type SleepCountMap = Map (ID, Minute) Int

main :: IO ()
main = do
    input <- readFile "day4.input"
    let Right records = sequenceA $ parseLine <$> sort (lines input)
    let (_, _, sleepCounts) = foldl' fillIDs (Nothing, Nothing, Map.empty) records
    let sleepSums = Map.mapKeysWith (+) fst sleepCounts
    let mostSleptGuardID = fst $ maximumBy (comparing snd) (Map.toList sleepSums)
    let mostSleptMinute =
            fst
                $ maximumBy (comparing snd)
                $ Map.toList
                $ Map.mapKeysWith (+) snd
                $ Map.filterWithKey (\k _ -> fst k == mostSleptGuardID) sleepCounts
    print $ "1: " <> show (mostSleptGuardID * mostSleptMinute)

    let (mostFequentID, mostFequentMinute) = fst $ maximumBy (comparing snd) $ Map.toList sleepCounts
    print $ "2: " <> show (mostFequentID * mostFequentMinute)

fillIDs :: (Maybe ID, Maybe Minute, SleepCountMap)
        -> (Maybe ID, Minute)
        -> (Maybe ID, Maybe Minute, SleepCountMap)
fillIDs (_, Nothing, sleepCounts) (Just newID, _) = (Just newID, Nothing, sleepCounts)
fillIDs (Just oldID, Just fallAsleepMinute, sleepCounts) (Just newID, _) = (Just newID, Nothing, newCounts)
    where newCounts = enterSleep sleepCounts oldID fallAsleepMinute 60
fillIDs (guardID, Nothing, sleepCounts) (_, fallAsleepMinute) = (guardID, Just fallAsleepMinute, sleepCounts)
fillIDs (Just guardID, Just fallAsleepMinute, sleepCounts) (_, wakeUpMinute) = (Just guardID, Nothing, newCounts)
    where newCounts = enterSleep sleepCounts guardID fallAsleepMinute wakeUpMinute

enterSleep :: SleepCountMap -> ID -> Minute -> Minute -> SleepCountMap
enterSleep mp guardID start end = Map.unionWith (+) mp newMp
  where
   newMp = Map.fromAscList asleepList
   asleepList = (\minute -> ((guardID, minute), 1)) <$> asleepMinutes
   asleepMinutes = [start..(end - 1)]

parseLine :: String -> Either P.ParseError (Maybe ID, Minute)
parseLine = P.parse parser ""
  where
    parser = do
        timeString <- P.manyTill P.anyChar P.space *> colonSep (P.many1 P.digit)
        let (hour:minute:_) = read <$> timeString
        let time = if hour == 23
                      then minute - 60
                      else minute

        idString <- P.optionMaybe (P.try (P.manyTill P.anyChar (P.char '#') *> P.many1 P.digit))
        let id = read <$> idString

        return (id, time)
    colonSep p = p `P.sepBy` P.char ':'
