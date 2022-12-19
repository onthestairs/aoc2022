{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day19 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (maximum, maximumBy, nub, partition)
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

search n init step toSeen = go 0 Set.empty [init]
  where
    go t _ ss | t == n = ss
    go t seen ss = go (t + 1) m' ss'
      where
        ss' = ordNub $ concatMap (step seen) ss
        m' = Set.union seen (Set.fromList (map toSeen ss'))

mine n (oreCostOre, clayCostOre, (obsidianCostOre, obsidianCostClay), (geodeCostOre, geodeCostObsidian)) = maximum $ map (fth4 . fst) $ search n ((0, 0, 0, 0), (1, 0, 0, 0)) step snd
  where
    maxOreCost = maximum [oreCostOre, clayCostOre, obsidianCostOre, geodeCostOre]
    step :: Set.Set (Int, Int, Int, Int) -> ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> [((Int, Int, Int, Int), (Int, Int, Int, Int))]
    step m ((ore, clay, obsidian, geode), (oreRobots, clayRobots, obsidianRobots, geodeRobots)) = nothingBuilt : builds
      where
        builds = filter (\(_, rs) -> Set.notMember rs m) (geodeBuilt <> obsidianBuilt <> clayBuilt <> oreBuilt)
        oreBuilt =
          [ ((ore + oreRobots - oreCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots + 1, clayRobots, obsidianRobots, geodeRobots))
            | ore >= oreCostOre,
              oreRobots <= maxOreCost,
              null obsidianBuilt || null geodeBuilt
          ]
        clayBuilt =
          [ ((ore + oreRobots - clayCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots + 1, obsidianRobots, geodeRobots))
            | ore >= clayCostOre,
              clayRobots <= obsidianCostClay,
              null obsidianBuilt || null geodeBuilt
          ]
        obsidianBuilt =
          [ ((ore + oreRobots - obsidianCostOre, clay + clayRobots - obsidianCostClay, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots + 1, geodeRobots))
            | ore >= obsidianCostOre && clay >= obsidianCostClay,
              obsidianRobots <= geodeCostObsidian,
              null geodeBuilt
          ]
        geodeBuilt =
          [ ((ore + oreRobots - geodeCostOre, clay + clayRobots, obsidian + obsidianRobots - geodeCostObsidian, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots + 1))
            | ore >= geodeCostOre && obsidian >= geodeCostObsidian
          ]
        nothingBuilt =
          ((ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots))

fth4 (_, _, _, w) = w

snd3 (_, y, _) = y

solve1 = sum . map (\(id, costs) -> id * mine 24 costs)

t1, t2 :: (Int, Int, (Int, Int), (Int, Int))
t1 = (4, 2, (3, 14), (2, 7))
t2 = (2, 3, (3, 8), (3, 12))

solve2 = product . map (mine 32 . snd) . take 3

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
