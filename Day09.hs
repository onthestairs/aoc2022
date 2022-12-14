{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day09 (solution) where

import AOC (Parser, Solution (..), last', parseInt, sepByNewline)
import Data.List (nub)
import Relude
import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (char, newline)

data Dir = R | L | U | D deriving (Show)

type Input = [(Dir, Int)]

parseDir = char 'R' $> R <|> char 'L' $> L <|> char 'U' $> U <|> char 'D' $> D

parseMove = do
  d <- parseDir
  char ' '
  n <- parseInt
  pure (d, n)

parseInput :: Parser Input
parseInput = sepByNewline parseMove

move R (x, y) = (x + 1, y)
move L (x, y) = (x - 1, y)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)

distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

moveTowards (x1, y1) (x2, y2)
  | distance (x1, y1) (x2, y2) <= 1 = (x2, y2)
  | otherwise = (x2 + signum (x1 - x2), y2 + signum (y1 - y2))

moveRope [] _ = []
moveRope (h : ks) dir = h' : ks'
  where
    h' = move dir h
    ks' = zipWith moveTowards (h' : ks') ks

flattenMoves = concatMap (\(d, n) -> replicate n d)

solve rope = length . nub . map last' . scanl moveRope rope . flattenMoves

solve1 = solve (replicate 2 (0, 0))

solve2 = solve (replicate 10 (0, 0))

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
