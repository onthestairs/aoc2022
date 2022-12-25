{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day25 (solution) where

import AOC (NoPuzzle (NoPuzzle), Parser, Solution (..), parseInt, sepByNewline)
import Data.List (nub)
import Relude
import Relude.Extra (Foldable1 (maximum1), average1)
import Text.Megaparsec (choice, eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline)

type Input = [[Char]]

parseSNAFU = many $ choice [two, one, zero, minus, doubleMinus]
  where
    two = char '2'
    one = char '1'
    zero = char '0'
    minus = char '-'
    doubleMinus = char '='

parseInput :: Parser Input
parseInput = sepByNewline parseSNAFU

snafuToDec :: [Char] -> Integer
snafuToDec ns = sum $ zipWith f (reverse ns) [0 ..]
  where
    f n i = toI n * 5 ^ i
    toI '2' = 2
    toI '1' = 1
    toI '0' = 0
    toI '-' = -1
    toI '=' = -2
    toI _ = error ""

decToSnafu = reverse . go
  where
    go 0 = ""
    go n = case r of
      0 -> '0' : go d
      1 -> '1' : go d
      2 -> '2' : go d
      3 -> '=' : go (d + 1)
      4 -> '-' : go (d + 1)
      _ -> error "cant be reached"
      where
        d = n `div` 5
        r = n `mod` 5

solve1 = toText . decToSnafu . sum . map snafuToDec

solve2 = const NoPuzzle

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
