{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day08 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseInt, sepByNewline)
import Data.List (nub)
import Data.Matrix (Matrix, fromLists, mapPos, safeGet, toLists, transpose)
import Data.Time.Format.ISO8601 (intervalFormat)
import Relude hiding (transpose)
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = Matrix Int

parseInput :: Parser Input
parseInput = fromLists <$> sepByNewline (many parseDigit)

checkRow _ [] = []
checkRow max ((x, c) : xs) = if x <= max then checkRow max xs else c : checkRow x xs

checkRows = concatMap (checkRow (-1)) . toLists

rotate :: Matrix a -> Matrix a
rotate = fromLists . reverse . toLists . transpose

rotations :: Matrix a -> [Matrix a]
rotations = take 4 . iterate rotate

m :: Matrix Int
m = fromLists [[3, 0, 3, 7, 3], [2, 5, 5, 1, 2], [6, 5, 3, 3, 2], [3, 3, 5, 4, 9], [3, 5, 3, 9, 0]]

annotateCoords = mapPos (\c x -> (x, c))

solve1 = length . nub . concatMap checkRows . rotations . annotateCoords

scenicScore :: Matrix Int -> (Int, Int) -> Int -> Int
scenicScore m c n = scoreN m c n * scoreW m c n * scoreS m c n * scoreE m c n

scoreN m (row, col) n = score
  where
    norths = catMaybes $ takeWhile isJust $ map (\i -> safeGet (row - i) col m) [1 ..]
    trees = length $ takeWhile (< n) norths
    score = if trees == length norths then trees else trees + 1

scoreE m (row, col) n = score
  where
    easts = catMaybes $ takeWhile isJust $ map (\i -> safeGet row (col + i) m) [1 ..]
    trees = length $ takeWhile (< n) easts
    score = if trees == length easts then trees else trees + 1

scoreS m (row, col) n = score
  where
    souths = catMaybes $ takeWhile isJust $ map (\i -> safeGet (row + i) col m) [1 ..]
    trees = length $ takeWhile (< n) souths
    score = if trees == length souths then trees else trees + 1

scoreW m (row, col) n = score
  where
    wests = catMaybes $ takeWhile isJust $ map (\i -> safeGet row (col - i) m) [1 ..]
    trees = length $ takeWhile (< n) wests
    score = if trees == length wests then trees else trees + 1

solve2 :: Input -> Maybe Int
solve2 = viaNonEmpty maximum1 . toList . uncurry mapPos . (scenicScore &&& id)

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
