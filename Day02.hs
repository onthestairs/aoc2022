{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day02 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data T1 = A | B | C deriving (Show)

data T2 = X | Y | Z deriving (Show)

data Choice = Rock | Paper | Scissors

data Result = Win | Loss | Draw

type Input = [(T1, T2)]

parseT1 :: Parser T1
parseT1 = (char 'A' $> A) <|> (char 'B' $> B) <|> (char 'C' $> C)

parseT2 :: Parser T2
parseT2 = (char 'X' $> X) <|> (char 'Y' $> Y) <|> (char 'Z' $> Z)

parseGame = do
  t1 <- parseT1
  char ' '
  t2 <- parseT2
  pure (t1, t2)

parseInts :: Parser Input
parseInts = sepByNewline parseGame

t1ToChoice A = Rock
t1ToChoice B = Paper
t1ToChoice C = Scissors

t2ToChoice1 X = Rock
t2ToChoice1 Y = Paper
t2ToChoice1 Z = Scissors

t2ToChoice2 (A, X) = Scissors
t2ToChoice2 (A, Y) = Rock
t2ToChoice2 (A, Z) = Paper
t2ToChoice2 (B, X) = Rock
t2ToChoice2 (B, Y) = Paper
t2ToChoice2 (B, Z) = Scissors
t2ToChoice2 (C, X) = Paper
t2ToChoice2 (C, Y) = Scissors
t2ToChoice2 (C, Z) = Rock

choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

play Rock Rock = Draw
play Rock Paper = Loss
play Rock Scissors = Win
play Paper Rock = Win
play Paper Paper = Draw
play Paper Scissors = Loss
play Scissors Rock = Loss
play Scissors Paper = Win
play Scissors Scissors = Draw

score Win = 6
score Draw = 3
score Loss = 0

gameScore = score . uncurry play . swap

playRPS = sum . sequence [gameScore, choiceScore . snd]

mapTuple f g x = (f x, g x)

playGame t2F = playRPS . ((t1ToChoice . fst) &&& t2F)

solve1 = sum . map (playGame (t2ToChoice1 . snd))

solve2 = sum . map (playGame t2ToChoice2)

solution =
  Solution
    { _parse = parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
