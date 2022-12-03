{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (letterChar, newline)

type Input = [[Char]]

parseInput :: Parser Input
parseInput = sepByNewline (many letterChar)

findCommon xs ys = viaNonEmpty head $ filter (`elem` ys) xs

forceMaybe (Just x) = x
forceMaybe Nothing = error "no value"

splitInHalf xs = splitAt (length xs `div` 2) xs

itemType = uncurry findCommon . splitInHalf

priority c = if c >= 'a' then ord c - 96 else ord c - 38

solve1 = sum . map (priority . forceMaybe . itemType)

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

findCommon3 xs ys zs = viaNonEmpty head $ filter (\x -> x `elem` ys && x `elem` zs) xs

badge [xs, ys, zs] = findCommon3 xs ys zs

solve2 = sum . map (priority . forceMaybe . badge) . chunk 3

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
