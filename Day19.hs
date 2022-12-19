{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day19 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (maximum, maximumBy, nub)
import qualified Data.Set as Set
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (lowerChar, newline, string)

type Input = [(Int, (Int, Int, (Int, Int), (Int, Int)))]

parseBlueprint = do
  string "Blueprint "
  id <- parseInt
  string ": Each ore robot costs "
  oreCost <- parseInt
  string " ore. Each clay robot costs "
  clayCost <- parseInt
  string " ore. Each obsidian robot costs "
  obsidianCostOre <- parseInt
  string " ore and "
  obsidianCostClay <- parseInt
  string " clay. Each geode robot costs "
  geodeCostOre <- parseInt
  string " ore and "
  geodeCostObsidian <- parseInt
  string " obsidian."
  pure (id, (oreCost, clayCost, (obsidianCostOre, obsidianCostClay), (geodeCostOre, geodeCostObsidian)))

parseInput :: Parser Input
parseInput = sepByNewline parseBlueprint

dfs :: (Ord a, Show a) => (a -> [a]) -> [a] -> [a]
dfs next = go Set.empty
  where
    go seen [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go seen' (next x ++ xs)
      where
        seen' = Set.insert x seen

mine :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> [(Int, (Int, Int, Int, Int), (Int, Int, Int, Int))]
mine n (oreCostOre, clayCostOre, (obsidianCostOre, obsidianCostClay), (geodeCostOre, geodeCostObsidian)) = dfs step [(0, (0, 0, 0, 0), (1, 0, 0, 0))]
  where
    step :: (Int, (Int, Int, Int, Int), (Int, Int, Int, Int)) -> [(Int, (Int, Int, Int, Int), (Int, Int, Int, Int))]
    step (i, _, _) | i == n = []
    step (i, (ore, clay, obsidian, geode), (oreRobots, clayRobots, obsidianRobots, geodeRobots)) = map (\(cs, rs) -> (i + 1, cs, rs)) (oreBought ++ clayBought ++ obsidianBought ++ geodeBought ++ nothingBought)
      where
        oreBought = [((ore + oreRobots - oreCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots + 1, clayRobots, obsidianRobots, geodeRobots)) | ore >= oreCostOre, oreRobots <= maximum [oreCostOre, clayCostOre, obsidianCostOre, geodeCostOre]]
        clayBought = [((ore + oreRobots - clayCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots + 1, obsidianRobots, geodeRobots)) | ore >= clayCostOre, clayRobots <= obsidianCostClay]
        obsidianBought = [((ore + oreRobots - obsidianCostOre, clay + clayRobots - obsidianCostClay, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots + 1, geodeRobots)) | ore >= obsidianCostOre && clay >= obsidianCostClay, obsidianRobots <= geodeCostObsidian]
        geodeBought = [((ore + oreRobots - geodeCostOre, clay + clayRobots, obsidian + obsidianRobots - geodeCostObsidian, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots + 1)) | ore >= geodeCostOre && obsidian >= geodeCostObsidian]
        nothingBought = [((ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots))]

fth4 (_, _, _, w) = w

snd3 (_, y, _) = y

maxGeodes :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> Int
maxGeodes n costs = maximum $ map (fth4 . snd3) $ mine n costs

solve1 :: Input -> Int
solve1 = sum . traceShowId . map (\(id, costs) -> id * maxGeodes 24 costs)

t1, t2 :: (Int, Int, (Int, Int), (Int, Int))
t1 = (4, 2, (3, 14), (2, 7))
t2 = (2, 3, (3, 8), (3, 12))

solve2 = sum . traceShowId . map (maxGeodes 32 . snd) . take 3

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
