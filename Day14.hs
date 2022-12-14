{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline, tail')
import Data.List (elemIndex, findIndex, maximum)
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (between, sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

type Input = [[(Int, Int)]]

parseCoord = do
  x <- parseInt
  char ','
  y <- parseInt
  pure (x, y)

parseRock = sepBy1 parseCoord (string " -> ")

parseInput :: Parser Input
parseInput = sepByNewline parseRock

range x1 x2 = if x1 >= x2 then [x2 .. x1] else [x1 .. x2]

interpolate (x1, y1) (x2, y2) = [(x, y) | x <- range x1 x2, y <- range y1 y2]

buildRock es = concat $ zipWith interpolate es (tail' es)

buildCoords = Set.fromList . concatMap buildRock

data Grid = Grid
  { rocks :: Set.Set (Int, Int),
    maxY :: Int
  }
  deriving (Show)

buildGrid rs = Grid {rocks = cs, maxY = maxY}
  where
    cs = buildCoords rs
    maxY = maximum $ map snd $ Set.toList cs

findNextCoord (x, y) os
  | not $ Set.member (x, y + 1) os = (x, y + 1)
  | not $ Set.member (x - 1, y + 1) os = (x - 1, y + 1)
  | not $ Set.member (x + 1, y + 1) os = (x + 1, y + 1)
  | otherwise = (x, y)

dropSand (x, y) maxY os
  | y > maxY = Nothing
  | otherwise = if nextCoord == (x, y) then Just (x, y) else dropSand nextCoord maxY os
  where
    nextCoord = findNextCoord (x, y) os

dropSands (Grid rocks maxY) = go Set.empty
  where
    go sands = case dropSand (500, 0) maxY (Set.union rocks sands) of
      Just settled -> go (Set.insert settled sands)
      Nothing -> sands

solve1 = length . dropSands . buildGrid

findNextCoord2 floorY (x, y) os
  | y + 1 == floorY = (x, y)
  | not $ Set.member (x, y + 1) os = (x, y + 1)
  | not $ Set.member (x - 1, y + 1) os = (x - 1, y + 1)
  | not $ Set.member (x + 1, y + 1) os = (x + 1, y + 1)
  | otherwise = (x, y)

dropSand2 (x, y) maxY os = if nextCoord == (x, y) then Just (x, y) else dropSand2 nextCoord maxY os
  where
    nextCoord = findNextCoord2 (maxY + 2) (x, y) os

dropSands2 (Grid rocks maxY) = go Set.empty
  where
    go sands = case dropSand2 (500, 0) maxY (Set.union rocks sands) of
      Just (500, 0) -> Set.insert (500, 0) sands
      Just settled -> go (Set.insert settled sands)
      Nothing -> sands

solve2 = length . dropSands2 . buildGrid

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
