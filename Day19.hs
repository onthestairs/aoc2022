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

-- ordNub :: Ord a => [a] -> [a]
-- ordNub xs = foldr f (const []) xs Set.empty
--   where
--     f x rec seen =
--       case rec <$> Set.alterF (\old -> (old, True)) x seen of
--         (True, ys) -> ys
--         (False, ys) -> x : ys

-- bfs :: (Ord a, Show a) => (a -> [a]) -> [a] -> [a]
-- bfs :: (Ord a, Show a) => s -> (s -> a -> Bool) -> (s -> [a] -> s) -> (a -> Bool) -> (a -> [a]) -> [a] -> [a]
-- bfs m prune update done next start = go m start
--   where
--     go m ss = if null ds then go m' us else ds
--       where
--         ss' = filter (prune m) $ ordNub $ concatMap next ss
--         (ds, us) = partition done ss'
--         m' = update m ss'
--
-- mine :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> [(Int, (Int, Int, Int, Int), (Int, Int, Int, Int))]
-- mine n (oreCostOre, clayCostOre, (obsidianCostOre, obsidianCostClay), (geodeCostOre, geodeCostObsidian)) = bfs (Set.singleton (0, 0, 0, 0)) prune update done step [(0, (0, 0, 0, 0), (1, 0, 0, 0))]
--   where
--     prune m (_, _, robots) = Set.notMember robots m
--     update m ss = Set.union m (Set.fromList $ (map (\(_, _, robots) -> robots) ss))
-- done (i, _, _) = i == n

mine n b@(oreCostOre, clayCostOre, (obsidianCostOre, obsidianCostClay), (geodeCostOre, geodeCostObsidian)) = solve n b ((0, 0, 0, 0), (1, 0, 0, 0))
  where
    solve :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> Int
    solve limit blue st0 = go 0 (Set.singleton (0, 0, 0, 0)) [st0]
      where
        go t _ sts | t == limit = maximum (map (fth4 . fst) sts)
        go t seen sts = go (t + 1) (Set.union seen (Set.fromList (map rep sts'))) sts'
          where
            sts' =
              map mine' sts
                ++ filter (\x -> rep x `Set.notMember` seen) (ordNub (concatMap (step) sts))
    rep = snd
    maxOreCost = maximum [oreCostOre, clayCostOre, obsidianCostOre, geodeCostOre]
    mine' ((ore, clay, obsidian, geode), (oreRobots, clayRobots, obsidianRobots, geodeRobots)) = ((ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots))
    step :: ((Int, Int, Int, Int), (Int, Int, Int, Int)) -> [((Int, Int, Int, Int), (Int, Int, Int, Int))]
    step ((ore, clay, obsidian, geode), (oreRobots, clayRobots, obsidianRobots, geodeRobots)) = geodeBought ++ obsidianBought ++ clayBought ++ oreBought
      where
        oreBought =
          [ ((ore + oreRobots - oreCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots + 1, clayRobots, obsidianRobots, geodeRobots))
            | ore >= oreCostOre,
              oreRobots <= maxOreCost,
              -- ore <= (n - i) * maxOreCost,
              null obsidianBought || null geodeBought
          ]
        clayBought =
          [ ((ore + oreRobots - clayCostOre, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots + 1, obsidianRobots, geodeRobots))
            | ore >= clayCostOre,
              clayRobots <= obsidianCostClay,
              -- clay <= (n - i) * obsidianCostClay,
              null obsidianBought || null geodeBought
          ]
        obsidianBought =
          [ ((ore + oreRobots - obsidianCostOre, clay + clayRobots - obsidianCostClay, obsidian + obsidianRobots, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots + 1, geodeRobots))
            | ore >= obsidianCostOre && clay >= obsidianCostClay,
              obsidianRobots <= geodeCostObsidian,
              -- obsidian <= (n - i) * geodeCostObsidian,
              null geodeBought
          ]
        geodeBought =
          [ ((ore + oreRobots - geodeCostOre, clay + clayRobots, obsidian + obsidianRobots - geodeCostObsidian, geode + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots + 1))
            | ore >= geodeCostOre && obsidian >= geodeCostObsidian
          ]

-- (+++) :: [a] -> [a] -> [a]
-- [] +++ xs = xs
-- xs +++ _ = xs
--
-- infixr 5 +++

fth4 (_, _, _, w) = w

snd3 (_, y, _) = y

-- maxGeodes :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> Int
-- maxGeodes n costs = maximum $ map (fth4 . snd3) $ mine n costs
maxGeodes :: Int -> (Int, Int, (Int, Int), (Int, Int)) -> Int
maxGeodes n costs = mine n costs

solve1 :: Input -> Int
solve1 = sum . traceShowId . map (\(id, costs) -> id * maxGeodes 24 costs)

t1, t2 :: (Int, Int, (Int, Int), (Int, Int))
t1 = (4, 2, (3, 14), (2, 7))
t2 = (2, 3, (3, 8), (3, 12))

solve2 = product . traceShowId . map (maxGeodes 32 . snd) . take 3

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
