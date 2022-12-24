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

findBounds :: [[a]] -> (Int, Int, Int, Int)
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

findPath :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) [Dir] -> Int
findPath bounds start end startBs = go 0 startBs (Set.singleton start)
  where
    go :: Int -> Map.Map (Int, Int) [Dir] -> Set.Set (Int, Int) -> Int
    go n bs ss | Set.member end ss = n
    go n bs ss = go (n + 1) bs' ss'
      where
        bs' = stepBlizzards bounds bs
        ss' = Set.fromList $ concatMap step $ Set.toList ss
        step (x, y) = steps start end bounds bs' (x, y)

data B = BR | BL | BU | BD | M Int | W | E

instance Show B where
  show BR = ">"
  show BL = "<"
  show BU = "^"
  show BD = "v"
  show (M n) = Text.Show.show n
  show W = "#"
  show E = "."

drawBlizzards (minX, maxX, minY, maxY) bs = Matrix.matrix rs cs f
  where
    rs = maxY - minY + 1
    cs = maxX - minX + 1
    f (row, col) | col - 1 == minX = W
    f (row, col) | col - 1 == maxX = W
    f (row, col) | row - 1 == minX = W
    f (row, col) | row - 1 == maxY = W
    f (row, col) = case Map.lookup (col - 1, row - 1) bs of
      Just [d] -> case d of
        R -> BR
        L -> BL
        U -> BU
        D -> BD
      Just ds -> M $ length ds
      Nothing -> E

solve1 ts = findPath bounds start end blizzards
  where
    blizzards = toBlizzards ts
    bounds = traceShowId $ findBounds ts
    (start, end) = findStartEnd ts

check bounds 0 bs = []
check bounds n bs = traceShow (drawBlizzards bounds bs) $ Map.size bs : check bounds (n - 1) bs'
  where
    bs' = stepBlizzards bounds bs

solve2 ts = product $ traceShowId $ check bounds 3 blizzards
  where
    blizzards = toBlizzards ts
    bounds = traceShowId $ findBounds ts

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
