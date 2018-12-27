#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package sbv
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Prelude hiding (id)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty, optional)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.List (foldl', sortBy, partition, nub)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as M

type Parser = P.Parsec Void String
data DamageType = Slashing | Radiation | Fire | Cold | Bludgeoning deriving (Show, Read, Eq)
data DefenseType = Weakness | Immunity deriving (Show, Eq)
data GroupType = ImmuneSystem | Infection deriving (Show, Eq)
data Group = Group { unitCount :: !Int
                   , unitHP :: !Int
                   , weaknesses :: ![DamageType]
                   , immunities :: ![DamageType]
                   , damageType :: !DamageType
                   , damage :: !Int
                   , initiative :: !Int
                   , groupType :: !GroupType
                   , id :: !Int
                   } deriving (Show, Eq)
type State = M.Map Int Group

main :: IO ()
main = do
    Right state <- parseInput <$> readFile "day24.input"
    print $ sum $ unitCount <$> combat state
    print $ sum $ unitCount <$> firstWinningCombat state

firstWinningCombat :: State -> State
firstWinningCombat = binarySearch 0 1000000 f p
  where
    f n s = combat $ boost s n
    p s = not (any ((== Infection) . groupType) $ M.elems s)

binarySearch :: Int -> Int -> (Int -> State -> State) -> (State -> Bool) -> State -> State
binarySearch min max f p state
  | min == max = f min state
  | otherwise = binarySearch newMin newMax f p state
  where
    newState = f median state
    median = min + ((max - min) `quot` 2)
    (newMin, newMax)
      | p newState = (min, median)
      | otherwise = (median + 1, max)

boost :: State -> Int -> State
boost state additionalDamage = f <$> state
  where
    f g | groupType g == ImmuneSystem = g { damage = damage g + additionalDamage}
        | otherwise = g

combat :: State -> State
combat state
  | over = state
  | newState == state = state
  | otherwise = combat newState
  where
    newState = fight state
    over = length (nub $ groupType <$> M.elems state) < 2

fight :: State -> State
fight state = M.filter ((> 0) . unitCount) attackResult
  where
    attackResult = foldl' attack state $ sortBy attackOrder $ M.elems state
    attack curState attacker
      | unitCount curAttacker <= 0 = curState
      | Nothing <- defenderId = curState
      | Just dId <- defenderId = let defender = curState M.! dId
                                     unitsLost = damageTakenAgainst defender curAttacker `quot` unitHP defender
                                     newDefender = defender { unitCount = unitCount defender - unitsLost }
                                  in M.insert (id newDefender) newDefender curState
      where
        curAttacker = curState M.! id attacker
        defenderId = targetSelection M.!? id attacker
    attackOrder = comparing (Down . initiative)
    targetSelection :: M.Map Int Int
    targetSelection = snd selectionPhase
    selectionPhase = foldl' selectionF (M.elems state, M.empty) $ sortBy selectionOrder $ M.elems state
    selectionOrder = comparing (Down . effectivePower) <> comparing (Down . initiative)
    selectionF (remainingTargets, selectionResult) g =
        let (allies, enemies) = partition ((== groupType g) . groupType) remainingTargets
            (bestEnemy:rest) = sortBy (targetOrder g) enemies
         in case enemies of
              [] -> (remainingTargets, selectionResult)
              _  -> if bestEnemy `immuneAgainst` g
                       then (remainingTargets, selectionResult)
                       else (allies ++ rest, M.insert (id g) (id bestEnemy) selectionResult)
    targetOrder g =
        comparing (`immuneAgainst` g)
            <> comparing (Down . (`weakAgainst` g))
            <> comparing (Down . effectivePower)
            <> comparing (Down . initiative)

damageTakenAgainst :: Group -> Group -> Int
damageTakenAgainst defender attacker
  | defender `immuneAgainst` attacker = 0
  | defender `weakAgainst` attacker = 2 * effectivePower attacker
  | otherwise = effectivePower attacker
weakAgainst :: Group -> Group -> Bool
weakAgainst x against = damageType against `elem` weaknesses x
immuneAgainst :: Group -> Group -> Bool
immuneAgainst x against = damageType against `elem` immunities x

effectivePower :: Group -> Int
effectivePower g = unitCount g * damage g

parseInput :: String -> Either (P.ParseError Char Void) State
parseInput input = do
    immuneGroups <- parse ImmuneSystem 0 (drop 1 immuneLines)
    infectionGroups <- parse Infection (length immuneGroups) (drop 2 infectionLines)
    return $ M.fromList $ (\g -> (id g, g)) <$> (immuneGroups ++ infectionGroups)
  where
    parse groupType startId ls = sequenceA (uncurry (parseLine groupType) <$> zip [startId..] ls)
    (immuneLines, infectionLines) = break null (lines input)

parseLine :: GroupType -> Int -> String -> Either (P.ParseError Char Void) Group
parseLine groupType id = P.parse parser ""
  where
    parser = do
        unitCount <- read <$> (number <* string "units")
        unitHP <- read <$> (string "each with" *> number <* string "hit points")
        (weaknesses, immunities) <- fromMaybe ([], []) <$> optional defense
        damage <- read <$> (string "with an attack that does" *> number)
        damageType <- dmgTypeParser <* string "damage"
        initiative <- read <$> (string "at initiative" *> number)
        return (Group {..})
    number = lexeme (P.some PC.digitChar)
    string = L.symbol spaceConsumer
    spaceConsumer = L.space PC.space1 empty empty
    lexeme = L.lexeme spaceConsumer
    defense :: Parser ([DamageType], [DamageType])
    defense = processDefense <$> lexeme (P.between (PC.char '(') (PC.char ')') defenseTypeParser)
    processDefense defenses = let ws = concatMap snd $ filter ((== Weakness) . fst) defenses
                                  is = concatMap snd $ filter ((== Immunity) . fst) defenses
                               in (ws, is)
    defenseTypeParser :: Parser [(DefenseType, [DamageType])]
    defenseTypeParser = semiSep (P.try weakness P.<|> P.try immunity)
    weakness :: Parser (DefenseType, [DamageType])
    weakness = (Weakness, ) <$> (string "weak to" *> commaSep dmgTypeParser)
    immunity :: Parser (DefenseType, [DamageType])
    immunity = (Immunity, ) <$> (string "immune to" *> commaSep dmgTypeParser)
    dmgTypeParser :: Parser DamageType
    dmgTypeParser = readDamageType <$> (string "slashing"
                                  P.<|> string "radiation"
                                  P.<|> string "fire"
                                  P.<|> string "cold"
                                  P.<|> string "bludgeoning")
    readDamageType (x:xs) = read (toUpper x:xs)
    commaSep p = p `P.sepBy1` string ","
    semiSep p = p `P.sepBy1` string ";"
