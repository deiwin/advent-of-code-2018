#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Vector.Unboxed as VU
import Data.List.Split (chunksOf)
import Data.List (isPrefixOf)
import Data.Bits ((.&.), (.|.))

type Registers = VU.Vector Int
type Instruction = (Op, InstructionInput)
type InstructionInput = (Int, Int, Int)
type Sample = (Registers, InstructionInput, Registers)

data Op = Addr | Addi
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
        deriving (Show, Read, Eq, Ord, Enum)

main :: IO ()
main = do
    samples <- parseInput <$> readFile "day16.input"
    print $ countLikeThreeOpcodes samples

countLikeThreeOpcodes :: [Sample] -> Int
countLikeThreeOpcodes samples = length $ filter likeThreeOrMore samples
  where
    likeThreeOrMore s = likeCount s >= 3
    likeCount (before, input, after) = length $ filter ((== after) . exec before . (, input)) $ enumFrom Addr

exec :: Registers -> Instruction -> Registers
exec rs i@(_, (_, _, c)) = rs VU.// [(c, go i)]
  where
    go (Addr, (a, b, c)) = (rs VU.! a) + (rs VU.! b)
    go (Addi, (a, b, c)) = (rs VU.! a) + b
    go (Mulr, (a, b, c)) = (rs VU.! a) * (rs VU.! b)
    go (Muli, (a, b, c)) = (rs VU.! a) * b
    go (Banr, (a, b, c)) = (rs VU.! a) .&. (rs VU.! b)
    go (Bani, (a, b, c)) = (rs VU.! a) .&. b
    go (Borr, (a, b, c)) = (rs VU.! a) .|. (rs VU.! b)
    go (Bori, (a, b, c)) = (rs VU.! a) .|. b
    go (Setr, (a, _, c)) = rs VU.! a
    go (Seti, (a, _, c)) = a
    go (Gtir, (a, b, c)) = if a > (rs VU.! b) then 1 else 0
    go (Gtri, (a, b, c)) = if (rs VU.! a) > b then 1 else 0
    go (Gtrr, (a, b, c)) = if (rs VU.! a) > (rs VU.! b) then 1 else 0
    go (Eqir, (a, b, c)) = if a == (rs VU.! b) then 1 else 0
    go (Eqri, (a, b, c)) = if (rs VU.! a) == b then 1 else 0
    go (Eqrr, (a, b, c)) = if (rs VU.! a) == (rs VU.! b) then 1 else 0

parseInput :: String -> [Sample]
parseInput input = fmap parseSample $ takeWhile isSample $ fmap (take 3) $ chunksOf 4 $ lines input
  where
    isSample (head:_) = "Before: " `isPrefixOf` head
    parseSample [before, input, after] = let parsedBefore = read $ dropWhile (/= '[') before
                                             [opCode, a, b, c] = read <$> words input
                                             instructionInput = (a, b, c)
                                             parsedAfter = read $ dropWhile (/= '[') after
                                          in (parsedBefore, instructionInput, parsedAfter)
