{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day18 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (maximum, minimum, nub)
import qualified Data.Set as Set
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline)

type Input = [(Int, Int, Int)]

parseCoord = do
  x <- parseInt
  char ','
  y <- parseInt
  char ','
  z <- parseInt
  pure (x, y, z)

parseInput :: Parser Input
parseInput = sepByNewline parseCoord

neighbours (x, y, z) =
  Set.fromList
    [ (x + 1, y, z),
      (x - 1, y, z),
      (x, y - 1, z),
      (x, y + 1, z),
      (x, y, z - 1),
      (x, y, z + 1)
    ]

surface cs c = length $ Set.filter (\n -> Set.notMember n cs) $ neighbours c

surfaceArea cs = sum $ map (surface cs) $ Set.toList cs

solve1 = surfaceArea . Set.fromList

popSet s = case Set.lookupMin s of
  Just e -> Just (e, Set.delete e s)
  Nothing -> Nothing

onTheEdge outOfBounds = any (any outOfBounds . neighbours)

makeExposed outOfBounds cs = Set.unions $ filter (onTheEdge outOfBounds) $ foldl' f [] airCs
  where
    airCs = [c | x <- [0 .. 20], y <- [0 .. 20], z <- [0 .. 20], let c = (x, y, z), Set.notMember c cs]
    f rs c =
      if any (Set.member c) rs
        then rs
        else explore c : rs
    explore c = go Set.empty (Set.singleton c)
      where
        go seen toSee = case popSet toSee of
          Just (l, toSee') -> go (Set.insert l seen) (Set.union toSee' next)
            where
              next = Set.filter (\n -> not (outOfBounds n) && Set.notMember n cs && Set.notMember n seen) (neighbours l)
          Nothing -> seen

fst3 (x, _, _) = x

snd3 (_, y, _) = y

thd3 (_, _, z) = z

makeOutOfBoundsFunction cs = \(x, y, z) -> x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ
  where
    minX = minimum $ map fst3 $ Set.toList cs
    maxX = maximum $ map fst3 $ Set.toList cs
    minY = minimum $ map snd3 $ Set.toList cs
    maxY = maximum $ map snd3 $ Set.toList cs
    minZ = minimum $ map thd3 $ Set.toList cs
    maxZ = maximum $ map thd3 $ Set.toList cs

surfaceArea2 cs = sum $ map (surface cs) $ Set.toList cs
  where
    surface cs c = length $ Set.filter (\n -> Set.member n exposed || outOfBounds n) $ neighbours c
    outOfBounds = makeOutOfBoundsFunction cs
    exposed = makeExposed outOfBounds cs

solve2 = surfaceArea2 . Set.fromList

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
