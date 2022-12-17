{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day17 (solution) where

import AOC (Parser, Solution (..), head')
import Data.List (maximum, minimum)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Relude
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

simulateFall _ [] _ = error "run out of dirs"
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

states rocks dirs = go dirs Set.empty (cycle rocks)
  where
    go _ _ [] = error "run out of rock"
    go dirs placed (rock : rest) = state : go nextDirs nextPlaced rest
      where
        (nextPlaced, nextDirs) = placeRock dirs placed rock
        height = maxY nextPlaced
        -- 50 and 100 chosen somewhat arbitrarily to retain the
        -- current state
        -- possible improvement would be to make the dirs be equal to
        -- the cycle length (or use an index)
        topRows = takeRows 50 placed
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

fst4 (x, _, _, _) = x

-- look for a loop in the states
-- each state is approximately
-- (dir index, rock index, top of grid)
-- we look to see when that pattern re-occurs
calculateTotal n = go 1 Map.empty
  where
    go _ _ [] = error "no more rocks placed"
    go i _ ((height, ds, rock, grid) : hs') | i == n = height
    go i m ((height, ds, rock, grid) : hs') = case Map.lookup (ds, rock, grid) m of
      Just (height0, j) -> height + (diff * periods) + extraTilEnd
        where
          -- how much we have risen during the cycle period
          diff = height - height0
          -- how many rocks per cycle
          period = i - j
          -- how many rocks still to fill to reach n
          toFill = n - i
          -- how many cycles we will see before n
          periods = toFill `div` period
          -- how many rocks left over after we've filled in the cycles
          leftover = toFill `mod` period
          -- calculate the extra height we will see to reach n rocks
          heightTilEnd = fst4 $ head' $ drop (leftover - 1) hs'
          extraTilEnd = heightTilEnd - height
      Nothing -> go (i + 1) (Map.insert (ds, rock, grid) (height, i) m) hs'

solve n ds = calculateTotal n ss
  where
    ds' = intersperse D $ cycle ds
    ss = states rocks ds'

solve1 = solve 2022

solve2 = solve 1000000000000

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
