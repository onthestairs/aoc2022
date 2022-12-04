{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day04 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.Time.Format.ISO8601 (intervalFormat)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = [((Int, Int), (Int, Int))]

interval = do
  i1 <- parseInt
  char '-'
  i2 <- parseInt
  pure (i1, i2)

intervals = do
  i1 <- interval
  char ','
  i2 <- interval
  pure (i1, i2)

parseInput :: Parser Input
parseInput = sepByNewline intervals

hasTotalOverlap ((i1, i2), (j1, j2)) = (i1 >= j1 && i2 <= j2) || (j1 >= i1 && j2 <= i2)

solve1 = length . filter hasTotalOverlap

hasPartialOverlap ((i1, i2), (j1, j2)) = (i1 <= j1 && i2 >= j1) || (i1 <= j2 && i2 >= j2) || (j1 <= i1 && j2 >= i1) || (j1 <= i2 && j2 >= i2)

solve2 = length . filter hasPartialOverlap

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
