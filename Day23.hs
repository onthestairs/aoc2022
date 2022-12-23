{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day23 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (maximum, minimum, nub)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Relude hiding (round)
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline)

type Input = [String]

parseInput :: Parser Input
parseInput = sepByNewline (many (char '.' <|> char '#'))

toElves :: Input -> Set.Set (Int, Int)
toElves = Set.fromList . map (\(x, y, c) -> (x, y)) . filter (\(_, _, c) -> c == '#') . concat . zipWith (\y row -> zipWith (\x c -> (x, y, c)) [0 ..] row) [0 ..]

neighbours (x, y) = Set.fromList [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], not (dx == 0 && dy == 0)]

populatedNeighbours (x, y) es = Set.filter (`Set.member` es) ns
  where
    ns = neighbours (x, y)

mapFirst f [] = Nothing
mapFirst f (x : xs) = case f x of
  Just v -> Just v
  Nothing -> mapFirst f xs

moveAdjacent North (x, y) = [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)]
moveAdjacent South (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
moveAdjacent West (x, y) = [(x - 1, y), (x - 1, y - 1), (x - 1, y + 1)]
moveAdjacent East (x, y) = [(x + 1, y), (x + 1, y - 1), (x + 1, y + 1)]

move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move West (x, y) = (x - 1, y)
move East (x, y) = (x + 1, y)

isValidMove d (x, y) neighbours =
  if not (any (`Set.member` neighbours) (moveAdjacent d (x, y)))
    then Just $ move d (x, y)
    else Nothing

round elves moves = newElves
  where
    proposals = mapMaybe propose $ Set.toList elves
    propose from = if Set.null elfNeighbours then Nothing else mapFirst isGoodProposal moves
      where
        elfNeighbours = populatedNeighbours from elves
        isGoodProposal move = case isValidMove move from elfNeighbours of
          Just to -> Just (from, to)
          Nothing -> Nothing
    nextCounts = Map.fromListWith (+) $ map (\(from, to) -> (to, 1)) proposals
    goodMoves = filter f proposals
    f (_, to) = case Map.lookup to nextCounts of
      Just n -> n <= 1
      Nothing -> error "isnt in count map"
    newElves = foldl' (\es (from, to) -> Set.insert to $ Set.delete from es) elves goodMoves

data Move = North | South | West | East

rotate [] = []
rotate (x : xs) = xs ++ [x]

rounds n elves = go n moves elves
  where
    moves = [North, South, West, East]
    go 0 _ elves' = elves'
    go n' moves elves' = go (n' - 1) (rotate moves) newElves
      where
        newElves = round elves' moves

bounds es = ((xMin, yMin), (xMax, yMax))
  where
    xMin = minimum $ map fst es
    xMax = maximum $ map fst es
    yMin = minimum $ map snd es
    yMax = maximum $ map snd es

emptyTiles es = n - Set.size es
  where
    es' = Set.toList es
    ((xMin, yMin), (xMax, yMax)) = bounds es'
    n = (xMax - xMin + 1) * (yMax - yMin + 1)

t :: Set.Set (Int, Int)
t = Set.fromList [(2, 1), (3, 1), (2, 2), (2, 4), (3, 4)]

draw es = Matrix.matrix (yMax - yMin + 1) (xMax - xMin + 1) g
  where
    es' = Set.toList es
    ((xMin, yMin), (xMax, yMax)) = bounds es'
    g (row, col) = if Set.member (xMin + col - 1, yMin + row - 1) es then '#' else '.'

solve1 = emptyTiles . rounds 10 . toElves

solve2 = rounds 10 . toElves

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
