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

findNextCoord hasFloor maxY (x, y) os
  | hasFloor && (y + 1 == (maxY + 2)) = (x, y)
  | not $ Set.member (x, y + 1) os = (x, y + 1)
  | not $ Set.member (x - 1, y + 1) os = (x - 1, y + 1)
  | not $ Set.member (x + 1, y + 1) os = (x + 1, y + 1)
  | otherwise = (x, y)

dropSand hasFloor maxY (x, y) os
  | not hasFloor && y > maxY = Nothing
  | otherwise = if nextCoord == (x, y) then Just (x, y) else dropSand hasFloor maxY nextCoord os
  where
    nextCoord = findNextCoord hasFloor maxY (x, y) os

dropSands hasFloor (Grid rocks maxY) = go Set.empty
  where
    go sands = case dropSand hasFloor maxY (500, 0) (Set.union rocks sands) of
      Just (500, 0) -> Set.insert (500, 0) sands
      Just settled -> go (Set.insert settled sands)
      Nothing -> sands

solve1 = length . dropSands False . buildGrid

solve2 = length . dropSands True . buildGrid

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
