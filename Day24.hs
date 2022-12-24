{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day24 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', last', parseInt, sepByNewline)
import Data.List (elemIndex, findIndex, nub, (!!))
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (choice, eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline)
import Text.Show

data Dir = R | L | U | D deriving (Show, Eq)

data Tile = Wall | Blizzard Dir | Empty deriving (Show, Eq)

type Input = [[Tile]]

parseTile = choice [char '#' $> Wall, char '.' $> Empty, l, r, u, d]
  where
    l = char '<' $> Blizzard L
    r = char '>' $> Blizzard R
    u = char '^' $> Blizzard U
    d = char 'v' $> Blizzard D

parseInput :: Parser Input
parseInput = sepByNewline (many parseTile)

toBlizzards ts = Map.fromListWith (++) blizzardDirs
  where
    gridWithIndexes = concat . zipWith (\y row -> zipWith (\x c -> (x, y, c)) [0 ..] row) [0 ..] $ ts
    blizzardDirs = mapMaybe f gridWithIndexes
    f (x, y, Blizzard d) = Just ((x, y), [d])
    f _ = Nothing

findBounds ts = (0, length (head' ts) - 1, 0, length ts - 1)

findStartEnd ts = ((startX, 0), (endX, endY))
  where
    startX = forceMaybe $ elemIndex Empty (head' ts)
    endX = forceMaybe $ elemIndex Empty (last' ts)
    endY = length ts - 1

moveBlizzard (minX, maxX, _, _) (x, y) R = if x + 1 /= maxX then (x + 1, y) else (minX + 1, y)
moveBlizzard (minX, maxX, _, _) (x, y) L = if x - 1 /= minX then (x - 1, y) else (maxX - 1, y)
moveBlizzard (_, _, minY, maxY) (x, y) D = if y + 1 /= maxY then (x, y + 1) else (x, minY + 1)
moveBlizzard (_, _, minY, maxY) (x, y) U = if y - 1 /= minY then (x, y - 1) else (x, maxY - 1)

stepBlizzards bounds = Map.fromListWith (++) . concatMap move . Map.toList
  where
    move ((x, y), ds) = map (\d -> (moveBlizzard bounds (x, y) d, [d])) ds

steps start end (minX, maxX, minY, maxY) bs (x, y) = filter isNotBlizzard $ filter inBounds [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    inBounds (x', y') = (x', y') == start || (x', y') == end || (x' > minX && x' < maxX && y' > minY && y' < maxY)
    isNotBlizzard (x, y) = Map.notMember (x, y) bs

findPath bounds start end startBs = go 0 startBs (Set.singleton start)
  where
    go n bs ss | Set.member end ss = (n, bs)
    go n bs ss = go (n + 1) bs' ss'
      where
        bs' = stepBlizzards bounds bs
        ss' = Set.fromList $ concatMap step $ Set.toList ss
        step (x, y) = steps start end bounds bs' (x, y)

solve1 ts = fst $ findPath bounds start end blizzards
  where
    blizzards = toBlizzards ts
    bounds = findBounds ts
    (start, end) = findStartEnd ts

solve2 ts = n1 + n2 + n3
  where
    blizzards = toBlizzards ts
    bounds = findBounds ts
    (start, end) = findStartEnd ts
    (n1, b1) = findPath bounds start end blizzards
    (n2, b2) = findPath bounds end start b1
    (n3, _) = findPath bounds start end b2

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
