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

isRock os extraRock (x, y) = Set.member (x, y) os || extraRock (x, y)

findNextCoord os extraRock (x, y)
  | not $ isRock os extraRock (x, y + 1) = (x, y + 1)
  | not $ isRock os extraRock (x - 1, y + 1) = (x - 1, y + 1)
  | not $ isRock os extraRock (x + 1, y + 1) = (x + 1, y + 1)
  | otherwise = (x, y)

dropSand stopCondition extraRock (x, y) os
  | stopCondition (x, y) = Nothing
  | otherwise = if nextCoord == (x, y) then Just (x, y) else dropSand stopCondition extraRock nextCoord os
  where
    nextCoord = findNextCoord os extraRock (x, y)

dropSands stopCondition extraRock rocks source = go Set.empty
  where
    go sands = case dropSand stopCondition extraRock source (Set.union rocks sands) of
      Just settled | settled == source -> Set.insert source sands
      Just settled -> go (Set.insert settled sands)
      Nothing -> sands

solve extraRockMaker stopConditionMaker i = length $ dropSands stopCondition extraRock rocks (500, 0)
  where
    Grid {rocks = rocks, maxY = maxY} = buildGrid i
    extraRock = extraRockMaker maxY
    stopCondition = stopConditionMaker maxY

solve1 = solve extraRock stopCondition
  where
    extraRock _ _ = False
    stopCondition maxY (x, y) = y > maxY

solve2 = solve extraRock stopCondition
  where
    extraRock maxY (x, y) = y == maxY + 2
    stopCondition _ _ = False

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
