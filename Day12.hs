{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', sepByNewline)
import Data.Char (ord)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PatriciaTree
import Data.Graph.Inductive.Query.SP (spLength)
import qualified Data.Matrix as Matrix
import Relude
import Relude.Extra (Foldable1 (minimum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (alphaNumChar, newline)

type Input = [[Char]]

parseInput :: Parser Input
parseInput = sepByNewline (some alphaNumChar)

getAt m c = uncurry Matrix.getElem c m

isInBounds m (row, col) = row >= 1 && col >= 1 && row <= Matrix.nrows m && col <= Matrix.ncols m

neighbours m (row, col) = filter (isInBounds m) [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]

coords m = [(row, col) | row <- [1 .. Matrix.nrows m], col <- [1 .. Matrix.ncols m]]

mapJoin f xs = zip xs (map f xs)

coordToInt m (row, col) = row * Matrix.ncols m + col

toVal v = case v of
  'S' -> 'a'
  'E' -> 'z'
  _ -> v

isOneAway m c c' = ord v2 - ord v1 <= 1
  where
    v1 = toVal $ getAt m c
    v2 = toVal $ getAt m c'

makeEdges m = foldMap edges (coords m)
  where
    edges c = [(c, c', 1) | c' <- neighbours m c, isOneAway m c c']

makeIntEdges m = map (\(c1, c2, n) -> (coordToInt m c1, coordToInt m c2, n)) (makeEdges m)

makeNodes m = [(coordToInt m c, c) | c <- coords m]

makeGraph :: [(Int, (Int, Int))] -> [(Int, Int, Int)] -> PatriciaTree.Gr (Int, Int) Int
makeGraph = Graph.mkGraph

findCoords m ts = map fst $ filter snd $ Matrix.toList $ Matrix.mapPos (\c v -> (c, v `elem` ts)) m

solve startCs m = viaNonEmpty minimum1 $ mapMaybe (\start -> spLength start end g) starts
  where
    es = makeIntEdges m
    ns = makeNodes m
    g = makeGraph ns es
    starts = map (coordToInt m) $ findCoords m startCs
    end = coordToInt m $ head' $ findCoords m ['E']

solve1 = solve ['S'] . Matrix.fromLists

solve2 = solve ['S', 'a'] . Matrix.fromLists

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
