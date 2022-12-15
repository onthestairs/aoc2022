{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 (solution) where

import AOC (AOCShow (showResult), Parser, Solution (..), chunk, head', parseDigit, parseInt, parseNegativeInt, sepByNewline)
import qualified Data.Text as T
import Relude
import Text.Megaparsec (eof, oneOf, sepBy1)
import Text.Megaparsec.Char (newline, string)
import qualified Text.Show

data Instruction = Noop | Addx Int deriving (Show)

type Input = [Instruction]

noop = string "noop" $> Noop

addx = do
  string "addx "
  n <- parseNegativeInt <|> parseInt
  pure (Addx n)

parseInstruction = noop <|> addx

parseInput :: Parser Input
parseInput = sepByNewline parseInstruction

addWait (Addx n) = (Addx n, 1)
addWait i = (i, 0)

runInstruction n Noop = n
runInstruction n (Addx m) = n + m

runCycles n [] = [n]
runCycles n ((i, 0) : is) = n : runCycles m is
  where
    m = runInstruction n i
runCycles n ((i, w) : is) = n : runCycles n ((i, w - 1) : is)

solve1 = sum . map (uncurry (*) . head') . chunk 40 . drop 19 . zip [1 ..] . runCycles 1 . map addWait

display = zipWith (\c p -> if abs (c - p) <= 1 then '█' else ' ') [0 ..]

newtype Screen = Screen [[Char]]

instance AOCShow Screen where
  showResult (Screen pss) = T.stripEnd $ unlines $ map toText pss

solve2 = Screen . map display . chunk 40 . take 240 . runCycles 1 . map addWait

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
