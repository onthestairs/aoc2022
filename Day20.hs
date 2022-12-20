{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day20 (solution) where

import AOC (Parser, Solution (..), forceMaybe, parseInt, parseNegativeInt, sepByNewline)
import Data.List (findIndex, (!!))
import Relude
import Relude.Extra (Foldable1 (maximum1))

type Input = [Int]

parseInput :: Parser Input
parseInput = sepByNewline (parseInt <|> parseNegativeInt)

popIndex xs i = (deleted, before ++ after)
  where
    (before, deleted, after) = case splitAt i xs of
      (b, x : a) -> (b, x, a)
      (b, []) -> error "none"

move :: [a] -> Int -> Int -> [a]
move ns i j = before ++ [n] ++ after
  where
    (n, ns') = popIndex ns i
    (before, after) = splitAt j ns'

mix ns = foldl' f ns [0 .. length ns - 1]
  where
    f ns' originalIndex = move ns' currentIndex targetIndex
      where
        currentIndex = forceMaybe $ findIndex ((==) originalIndex . fst) ns'
        (_, n) = forceMaybe $ find ((==) originalIndex . fst) ns'
        targetIndex' = (currentIndex + n) `mod` (length ns - 1)
        targetIndex = if targetIndex' == 0 then length ns - 1 else targetIndex'

wrappedLookup xs i = (!!) xs i'
  where
    i' = i `mod` length xs

extractResult ns = sum $ map (snd . wrappedLookup ns . (+ zeroIndex)) indexes
  where
    indexes = [1000, 2000, 3000]
    zeroIndex = forceMaybe $ findIndex ((==) 0 . snd) ns

solve1 = extractResult . mix . zip [0 ..]

iterateN 0 f = id
iterateN n f = f . iterateN (n - 1) f

solve2 = extractResult . iterateN 10 mix . zip [0 ..] . map (* 811589153)

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
