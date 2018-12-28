#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Control.Lens as L
import Control.Lens (makeLenses, lens, (&), (^.), (^?), (^?!), (.~), ix, (%~))
import Text.Show.Functions ()
import Data.Char (toUpper)
import Data.Bits ((.&.), (.|.))
import Data.Either (isRight)

import Debug.Trace (trace)

type Instruction = (Op, (Int, Int, Int))
data Op = Addr | Addi
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
        deriving (Show, Read, Eq, Ord)

data State = State { _instructionRegisterIx :: !Int
                   , _registers :: !(VU.Vector Int)
                   , _instructions :: !(V.Vector Instruction)
                   } deriving (Show)
$(makeLenses ''State)

main :: IO ()
main = do
    initialState <- parseInput <$> readFile "day21.input"
    let Left finalState = whileRight tick (initialState & register 0 .~ 0)
    print $ finalState ^. registers

whileRight :: (a -> Either b a) -> a -> Either b a
whileRight f x | (Right a) <- newX = whileRight f a
               | otherwise         = newX
    where newX = f x

tick :: State -> Either State State
tick state
  | Nothing <- instruction = Left state
  | Just i <- instruction = Right $ exec state i
  where
    instruction = (\i -> state ^? instructions . ix i) $ state ^?! instructionRegister

exec :: State -> Instruction -> State
exec s i | trace (show (s ^. registers, i)) False = undefined
exec s i = go i & overInstructionRegister (+ 1)
  where
    go (Addr, (a, b, c)) = s & register c .~ ((s ^?! register a) + (s ^?! register b))
    go (Addi, (a, b, c)) = s & register c .~ ((s ^?! register a) + b)
    go (Mulr, (a, b, c)) = s & register c .~ ((s ^?! register a) * (s ^?! register b))
    go (Muli, (a, b, c)) = s & register c .~ ((s ^?! register a) * b)
    go (Banr, (a, b, c)) = s & register c .~ ((s ^?! register a) .&. (s ^?! register b))
    go (Bani, (a, b, c)) = s & register c .~ ((s ^?! register a) .&. b)
    go (Borr, (a, b, c)) = s & register c .~ ((s ^?! register a) .|. (s ^?! register b))
    go (Bori, (a, b, c)) = s & register c .~ ((s ^?! register a) .|. b)
    go (Setr, (a, _, c)) = s & register c .~ (s ^?! register a)
    go (Seti, (a, _, c)) = s & register c .~ a
    go (Gtir, (a, b, c)) = s & register c .~ (if a > (s ^?! register b) then 1 else 0)
    go (Gtri, (a, b, c)) = s & register c .~ (if (s ^?! register a) > b then 1 else 0)
    go (Gtrr, (a, b, c)) = s & register c .~ (if (s ^?! register a) > (s ^?! register b) then 1 else 0)
    go (Eqir, (a, b, c)) = s & register c .~ (if a == (s ^?! register b) then 1 else 0)
    go (Eqri, (a, b, c)) = s & register c .~ (if (s ^?! register a) == b then 1 else 0)
    go (Eqrr, (a, b, c)) = s & register c .~ (if (s ^?! register a) == (s ^?! register b) then 1 else 0)

getInstructionRegister :: State -> Maybe Int
getInstructionRegister state = state ^? register (state ^. instructionRegisterIx)
overInstructionRegister :: (Int -> Int) -> State -> State
overInstructionRegister f state = state & register i %~ f where i = state ^. instructionRegisterIx

instructionRegister :: Applicative f => (Int -> f Int) -> State -> f State
instructionRegister = lens get set
  where
    get state = state ^?! register (state ^. instructionRegisterIx)
    set state x = state & register (state ^. instructionRegisterIx) .~ x

register :: Applicative f => Int -> (Int -> f Int) -> State -> f State
register n = registers . ix n

parseInput :: String -> State
parseInput input = State i registers instructions
 where
   i = read $ drop 4 $ head ls
   ls = lines input
   registers = VU.replicate 6 0
   instructions = V.fromList $ toInstruction . words <$> tail ls
   toInstruction (op : rest) = (parseOp op, tuplify3 (read <$> rest))
   parseOp (x:xs) = read (toUpper x : xs)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
