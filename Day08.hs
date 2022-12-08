{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day08 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseInt, sepByNewline)
import Data.List (nub)
import Data.Matrix (Matrix, fromLists, mapPos, safeGet, toLists, transpose)
import Data.Time.Format.ISO8601 (intervalFormat)
import Relude hiding (get, transpose)
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = Matrix Int

parseInput :: Parser Input
parseInput = fromLists <$> sepByNewline (many parseDigit)

checkRow _ [] = []
checkRow max ((x, c) : xs) = if x <= max then checkRow max xs else c : checkRow x xs

checkRows = concatMap (checkRow (-1)) . toLists

rotate = fromLists . reverse . toLists . transpose

rotations = take 4 . iterate rotate

annotateCoords = mapPos (\c x -> (x, c))

solve1 = length . nub . concatMap checkRows . rotations . annotateCoords

scenicScore m c n = scoreN m c n * scoreW m c n * scoreS m c n * scoreE m c n

get m (row, col) = safeGet row col m

score nextF m (row, col) n = score
  where
    nexts = catMaybes $ takeWhile isJust $ map (get m . nextF (row, col)) [1 ..]
    trees = length $ takeWhile (< n) nexts
    -- we need to add the final tree if it exists, but not if we hit an edge
    score = if trees == length nexts then trees else trees + 1

scoreN = score (\(row, col) i -> (row - i, col))

scoreS = score (\(row, col) i -> (row + i, col))

scoreE = score (\(row, col) i -> (row, col + i))

scoreW = score (\(row, col) i -> (row, col - i))

solve2 = viaNonEmpty maximum1 . toList . uncurry mapPos . (scenicScore &&& id)

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
