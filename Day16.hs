{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day16 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', parseInt, sepByNewline, tail')
import qualified Data.Graph.Inductive as Graph
import Data.List (elemIndex, findIndex, intersect, maximum, maximumBy, nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import Relude
import Text.Megaparsec (between, sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline, string, upperChar)

type Input = Map.Map String (Int, [String])

parseValveName = many upperChar

parseValve = do
  string "Valve "
  valve <- parseValveName
  string " has flow rate="
  rate <- parseInt
  (string "; tunnels lead to valves ") <|> (string "; tunnel leads to valve ")
  links <- sepBy1 parseValveName (string ", ")
  pure (valve, (rate, links))

parseInput :: Parser Input
parseInput = Map.fromList <$> sepByNewline parseValve

solve1 = maximum . findBestPaths "AA" 30

findBestPaths start n graph = go [((start, Set.empty), 0)] n
  where
    go states 0 = Map.fromListWith max [(open, n) | ((_, open), n) <- states]
    go states t = go nextStates (t - 1)
      where
        nextStates = takeMaxes (concatMap step states)
        step ((valve, open), n) = moves ++ flips
          where
            moves = [((nextValve, open), n) | nextValve <- snd (graph Map.! valve)]
            flips =
              [ ((valve, Set.insert valve open), n + (t - 1) * pressure)
                | Set.notMember valve open,
                  let pressure = fst (graph Map.! valve),
                  pressure /= 0
              ]
    takeMaxes = Map.assocs . Map.fromListWith max

pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]

isDisjoint :: Ord a => Set a -> Set a -> Bool
isDisjoint a b = Set.null $ Set.intersection a b

solve2 i = maximum [p1 + p2 | ((path1, p1), (path2, p2)) <- pairs (Map.toList ps), isDisjoint path1 path2]
  where
    ps = findBestPaths "AA" 26 i

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
