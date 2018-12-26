#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (chunksOf)
import Data.List (isPrefixOf, foldl', partition)
import Data.Bits ((.&.), (.|.))

type Registers = VU.Vector Int
type Instruction = (Op, InstructionInput)
type InstructionInput = (Int, Int, Int)
type Sample = (Registers, Int, InstructionInput, Registers)

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
    (samples, code) <- parseInput <$> readFile "day16.input"
    print $ countLikeThreeOpcodes samples
    print $ executionResult samples code

executionResult :: [Sample] -> [[Int]] -> Int
executionResult samples code = finalRegisters VU.! 0
  where
    finalRegisters = foldl' exec initialRegisters instructions
    initialRegisters = VU.fromList [0, 0, 0, 0]
    opCodeMap = findOpcodes samples
    instructions = toInstruction <$> code
    toInstruction [opCode, a, b, c] = (opCodeMap M.! opCode, (a, b, c))

findOpcodes :: [Sample] -> M.Map Int Op
findOpcodes samples = M.map singular finalOpSets
  where
    singular ops = if S.size ops == 1
                      then head $ S.toList ops
                      else error "More than one for op"
    finalOpSets = head $ dropWhile notFinal $ iterate tick initialOpSets
    tick m = flip M.union m $ reduce $ partition ((== 1) . S.size . snd) $ M.assocs m
    reduce (finalOps, others) = let finishedOps = S.unions $ snd <$> finalOps
                                 in M.fromList $ remove finishedOps <$> others
    remove finishedOps (opCode, ops) = (opCode, ops S.\\ finishedOps)
    notFinal m = any (> 1) $ S.size <$> M.elems m
    initialOpSets = foldl' f M.empty samples
    f :: M.Map Int (S.Set Op) -> Sample -> M.Map Int (S.Set Op)
    f m s@(_, opCode, _, _) = M.insert opCode (newEntry m s) m
    newEntry m s@(_, opCode, _, _) = let ops = S.fromList (likeOps s)
                                      in maybe ops (S.intersection ops) (m M.!? opCode)

countLikeThreeOpcodes :: [Sample] -> Int
countLikeThreeOpcodes samples = length $ filter likeThreeOrMore samples
  where
    likeThreeOrMore s = likeCount s >= 3
    likeCount = length . likeOps

likeOps :: Sample -> [Op]
likeOps (before, _, input, after) = filter ((== after) . exec before . (, input)) $ enumFrom Addr

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

parseInput :: String -> ([Sample], [[Int]])
parseInput input = (parseSample <$> rawSamples, code)
  where
    (rawSamples, rawCode) = span isSample $ chunksOf 4 $ lines input
    code = fmap read . words <$> dropWhile null (concat rawCode)
    isSample (head:_) = "Before: " `isPrefixOf` head
    parseSample [before, input, after, _] = let parsedBefore = read $ dropWhile (/= '[') before
                                                [opCode, a, b, c] = read <$> words input
                                                instructionInput = (a, b, c)
                                                parsedAfter = read $ dropWhile (/= '[') after
                                             in (parsedBefore, opCode, instructionInput, parsedAfter)
