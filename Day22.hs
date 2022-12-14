{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day22 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (maximumBy, minimumBy, nub)
import qualified Data.Set as Set
import Distribution.Fields.Lexer (Token (CloseBrace))
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline)

data Move = F Int | CounterClockwise | Clockwise deriving (Eq, Show)

data Tile = Empty | Open | Solid deriving (Show)

type Input = ([[Tile]], [Move])

parseTile = char ' ' $> Empty <|> char '.' $> Open <|> char '#' $> Solid

parseTileRow = many parseTile

parseMove = F <$> parseInt <|> char 'L' $> CounterClockwise <|> char 'R' $> Clockwise

parseInput :: Parser Input
parseInput = do
  tiles <- sepByNewline parseTileRow
  newline
  newline
  moves <- many parseMove
  pure (tiles, moves)

makeTileSets :: [[Tile]] -> (Set.Set (Int, Int), Set.Set (Int, Int))
makeTileSets tss = foldl' add (Set.empty, Set.empty) iss
  where
    iss :: [((Int, Int), Tile)]
    iss = concat $ zipWith (\row ts -> zipWith (\col t -> ((row, col), t)) [1 ..] ts) [1 ..] tss
    add (os, cs) ((row, col), Open) = (Set.insert (row, col) os, cs)
    add (os, cs) ((row, col), Solid) = (os, Set.insert (row, col) cs)
    add (os, cs) _ = (os, cs)

minimumOnRow os r = minimumBy (comparing fst) $ Set.toList $ Set.filter (\(row, col) -> row == r) os

maximumOnRow os r = maximumBy (comparing fst) $ Set.toList $ Set.filter (\(row, col) -> row == r) os

minimumOnCol os c = minimumBy (comparing snd) $ Set.toList $ Set.filter (\(row, col) -> col == c) os

maximumOnCol os c = maximumBy (comparing fst) $ Set.toList $ Set.filter (\(row, col) -> col == c) os

data Facing = FLeft | FRight | FUp | FDown deriving (Show, Eq)

rotateCounterClockwise FLeft = FDown
rotateCounterClockwise FDown = FRight
rotateCounterClockwise FRight = FUp
rotateCounterClockwise FUp = FLeft

rotateClockwise FLeft = FUp
rotateClockwise FUp = FRight
rotateClockwise FRight = FDown
rotateClockwise FDown = FLeft

next (row, col) FLeft = (row, col - 1)
next (row, col) FRight = (row, col + 1)
next (row, col) FUp = (row - 1, col)
next (row, col) FDown = (row + 1, col)

wrapPlane' os ss (row, col) FLeft = maximumOnRow (Set.union os ss) row
wrapPlane' os ss (row, col) FRight = minimumOnRow (Set.union os ss) row
wrapPlane' os ss (row, col) FUp = maximumOnCol (Set.union os ss) col
wrapPlane' os ss (row, col) FDown = minimumOnCol (Set.union os ss) col

wrapPlane os ss c f = (wrapPlane' os ss c f, f)

stepPlane os ss pos facing = (nextP, nextFacing)
  where
    hopefulNextP = next pos facing
    (nextP, nextFacing) =
      if Set.notMember hopefulNextP os && Set.notMember hopefulNextP ss
        then wrapPlane os ss hopefulNextP facing
        else (hopefulNextP, facing)

move step os ss pos facing 0 = (pos, facing)
move step os ss pos facing n =
  if Set.member nextP ss
    then (pos, facing)
    else move step os ss nextP nextFacing (n - 1)
  where
    (nextP, nextFacing) = step os ss pos facing

doMoves wrap os ss ms = go (start, FRight) ms
  where
    start = minimumOnRow os 1
    go (pos, facing) [] = (pos, facing)
    go (pos, facing) (CounterClockwise : ms') = go (pos, rotateCounterClockwise facing) ms'
    go (pos, facing) (Clockwise : ms') = go (pos, rotateClockwise facing) ms'
    go (pos, facing) (F n : ms') = go (nextPos, nextFacing) ms'
      where
        (nextPos, nextFacing) = move wrap os ss pos facing n

password ((row, col), facing) = 1000 * row + 4 * col + facingScore facing
  where
    facingScore FRight = 0
    facingScore FDown = 1
    facingScore FLeft = 2
    facingScore FUp = 3

solve step (ts, ms) = password $ doMoves step openTiles solidTiles ms
  where
    (openTiles, solidTiles) = makeTileSets ts

solve1 :: Input -> Int
solve1 = solve stepPlane

-- right
stepCube os ss (row, col) FRight | col == 150, row >= 1, row <= 50 = ((101 + (50 - row), 100), FLeft)
stepCube os ss (row, col) FRight | col == 100, row >= 51, row <= 100 = ((50, 101 + (row - 51)), FUp)
stepCube os ss (row, col) FRight | col == 100, row >= 101, row <= 150 = ((1 + (150 - row), 150), FLeft)
stepCube os ss (row, col) FRight | col == 50, row >= 151, row <= 200 = ((150, 51 + (row - 151)), FUp)
-- down
stepCube os ss (row, col) FDown | row == 50, col >= 101, col <= 150 = ((51 + (col - 101), 100), FLeft)
stepCube os ss (row, col) FDown | row == 150, col >= 51, col <= 100 = ((151 + (col - 51), 50), FLeft)
stepCube os ss (row, col) FDown | row == 200, col >= 1, col <= 50 = ((1, 101 + (col - 1)), FDown)
-- left
stepCube os ss (row, col) FLeft | col == 51, row >= 1, row <= 50 = ((101 + (50 - row), 1), FRight)
stepCube os ss (row, col) FLeft | col == 51, row >= 51, row <= 100 = ((101, 1 + (row - 51)), FDown)
stepCube os ss (row, col) FLeft | col == 1, row >= 101, row <= 150 = ((1 + (150 - row), 51), FRight)
stepCube os ss (row, col) FLeft | col == 1, row >= 151, row <= 200 = ((1, 51 + (row - 151)), FDown)
-- up
stepCube os ss (row, col) FUp | row == 1, col >= 51, col <= 100 = ((151 + (col - 51), 1), FRight)
stepCube os ss (row, col) FUp | row == 101, col >= 1, col <= 50 = ((51 + (col - 1), 51), FRight)
stepCube os ss (row, col) FUp | row == 1, col >= 101, col <= 150 = ((200, 1 + (col - 101)), FUp)
-- otherwise
stepCube os ss (row, col) facing = (next (row, col) facing, facing)

solve2 = solve stepCube

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
