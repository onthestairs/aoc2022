{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day06 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (nub)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (lowerChar, newline)

type Input = [Char]

parseInput :: Parser Input
parseInput = many lowerChar

windows n [] = []
windows n xs = take n xs : windows n (drop 1 xs)

allUnique = uncurry (==) . (length &&& (length . nub))

findUniqueWindow n = (+ n) . length . takeWhile (not . allUnique) . windows n

solve1 = findUniqueWindow 4

solve2 = findUniqueWindow 14

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
