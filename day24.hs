#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package sbv
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty, optional)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

type Parser = P.Parsec Void String
data DamageType = Slashing | Radiation | Fire | Cold | Bludgeoning deriving (Show, Read)
data DefenseType = Weakness | Immunity deriving (Show, Eq)
data Group = Group { unitCount :: !Int
                   , unitHP :: !Int
                   , weaknesses :: ![DamageType]
                   , immunities :: ![DamageType]
                   , damageType :: !DamageType
                   , damage :: !Int
                   , initiative :: !Int
                   } deriving (Show)

main :: IO ()
main = do
    input <- parseInput <$> readFile "day24.input"
    print input

parseInput :: String -> Either (P.ParseError Char Void) ([Group], [Group])
parseInput input = do
    immuneGroups <- parse (drop 1 immuneLines)
    infectionGroups <- parse (drop 2 infectionLines)
    return (immuneGroups, infectionGroups)
  where
    parse ls = sequenceA (parseLine <$> ls)
    (immuneLines, infectionLines) = break null (lines input)

parseLine :: String -> Either (P.ParseError Char Void) Group
parseLine = P.parse parser ""
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
