{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day17 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head')
import Data.List (maximum, minimum)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (runParser)
import Text.Megaparsec.Char (char)
import Text.Show

data Dir = L | R | D deriving (Eq, Ord, Show)

type Input = [Dir]

parseInput :: Parser Input
parseInput = many ((char '>' $> R) <|> (char '<' $> L))

-- ####
rock0 = [(0, 0), (1, 0), (2, 0), (3, 0)]

-- .#.
-- ###
-- .#.
rock1 = [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)]

-- ..#
-- ..#
-- ###
rock2 = [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]

-- #
-- #
-- #
-- #
rock3 = [(0, 4), (0, 3), (0, 2), (0, 1)]

-- ##
-- ##
rock4 = [(0, 1), (1, 1), (0, 0), (1, 0)]

rocks = [rock0, rock1, rock2, rock3, rock4]

maxY coords | Set.null coords = 0
maxY coords = maximum $ map snd $ toList coords

minY coords = minimum $ map snd $ toList coords

maxX coords = maximum $ map fst $ toList coords

minX coords = minimum $ map fst $ toList coords

move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)
move D (x, y) = (x, y - 1)

-- py is where the lowest point of the rock should be
place rock (px, py) = map shift rock
  where
    yShift = py - minY rock
    xShift = px
    shift (x, y) = (x + xShift, y + yShift)

moveRock d = map (move d)

overlap rocks rock = any (`Set.member` rocks) rock || rockMin == 0 || minX rock < 0 || maxX rock > 6
  where
    rockMin = minY rock

simulateFall rocks (d : dirs) rock
  | d == D && overlap rocks nextRock = (Set.union rocks (Set.fromList rock), dirs)
  | overlap rocks nextRock = simulateFall rocks dirs rock
  | otherwise = simulateFall rocks dirs nextRock
  where
    nextRock = moveRock d rock

placeRock dirs rocks rock = (nextRocks, nextDirs)
  where
    startingPoint = (2, maxY rocks + 1 + 3)
    (nextRocks, nextDirs) = simulateFall rocks dirs (place rock startingPoint)

takeRows n rocks = Set.map (\(x, y) -> (x, n - (highest - y))) $ Set.filter (\(x, y) -> y >= highest - n) rocks
  where
    highest = maxY rocks

heights :: [[(Int, Int)]] -> [Dir] -> [(Int, [Dir], [(Int, Int)], Set.Set (Int, Int))]
heights rocks dirs = go dirs Set.empty (cycle rocks)
  where
    go dirs placed (rock : rest) = state : go nextDirs nextPlaced rest
      where
        (nextPlaced, nextDirs) = placeRock dirs placed rock
        height = maxY nextPlaced
        topRows = takeRows 50 placed
        -- state = traceShow (drawRocks topRows) $ (height, take 100 nextDirs, rock, topRows)
        state = (height, take 100 nextDirs, rock, topRows)

data Material = Rock | None

instance Show Material where
  show Rock = "#"
  show None = "."

drawRocks rs = Matrix.matrix rows cols f
  where
    cols = 7
    rows = maxY rs + 1
    f (row, col) = if Set.member (col - 1, rows - row + 1) rs then Rock else None

solve1 = viaNonEmpty head . drop 2021 . heights rocks . intersperse D . cycle

-- solve1 ds = solve 2022 ds
--

fst4 (x, _, _, _) = x

-- calculateTotal :: Int -> [(Int, [Dir], [(Int, Int)], Set.Set (Int, Int))] -> Int
calculateTotal n hs = go 1 Map.empty hs
  where
    -- go :: Int -> Map.Map ([Dir], [(Int, Int)], Set.Set (Int, Int)) Int -> [(Int, [Dir], [(Int, Int)], Set.Set (Int, Int))] -> Int
    go i m ((height, ds, rock, grid) : hs') = case Map.lookup (ds, rock, grid) m of
      Just (height0, j) -> height + (diff * periods) + extraTilEnd
        where
          diff = height - height0
          period = traceShowId $ i - j
          toFill = n - i
          periods = traceShowId $ toFill `div` period
          leftover = traceShowId $ toFill `mod` period
          heightTilEnd = fst4 $ head' $ drop (leftover - 1) hs'
          extraTilEnd = heightTilEnd - height
      Nothing -> go (i + 1) (Map.insert (ds, rock, grid) (height, i) m) hs'

solve n ds = calculateTotal n ss
  where
    ds' = intersperse D $ cycle ds
    ss = heights rocks ds'

p input = forceMaybe $ rightToMaybe $ runParser parseInput "" input

t = p ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

-- solve2 = heights rocks
solve2 ds = solve 1000000000000 ds

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
