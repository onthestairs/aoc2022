{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day15 (solution) where

import AOC (Parser, Solution (..), parseInt, parseNegativeInt, sepByNewline)
import Relude
import Text.Megaparsec.Char (newline, string)

type Input = [((Int, Int), (Int, Int))]

parseSensor = do
  string "Sensor at x="
  sx <- parseInt <|> parseNegativeInt
  string ", y="
  sy <- parseInt <|> parseNegativeInt
  string ": closest beacon is at x="
  bx <- parseInt <|> parseNegativeInt
  string ", y="
  by <- parseInt <|> parseNegativeInt
  pure ((sx, sy), (bx, by))

parseInput :: Parser Input
parseInput = sepByNewline parseSensor

md (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

toRange (s, b) = (s, md s b)

makeRowInterval row ((bx, by), d) = if heightToRow > d then Nothing else Just interval
  where
    heightToRow = abs (row - by)
    rowWidth = d - heightToRow
    interval = (bx - rowWidth, bx + rowWidth)

overlap (i11, i12) (i21, i22) = i21 <= i12

normalise' ((i11, i12) : (i21, i22) : is) =
  if overlap (i11, i12) (i21, i22)
    then normalise ((min i11 i21, max i12 i22) : is)
    else (i11, i12) : normalise ((i21, i22) : is)
normalise' is = is

normalise = normalise' . sort

iLength (x1, x2) = x2 - x1

positionsInRowWithoutBeacon row bs = sum $ map iLength $ normalise $ mapMaybe (makeRowInterval row) bs

solve1 = positionsInRowWithoutBeacon 2_000_000 . map toRange

makeBoundedRowInterval row lower upper ((bx, by), d) = if heightToRow > d then Nothing else Just interval
  where
    heightToRow = abs (row - by)
    rowWidth = d - heightToRow
    interval = (max lower (bx - rowWidth), min upper (bx + rowWidth))

tuningFreq (x, y) = x * 4_000_000 + y

extractCoord (r, (i11, i12) : is) = (i12 + 1, r)

findRowWithMultipleIntervals upper rs = tuningFreq . extractCoord <$> foundIs
  where
    foundIs = find ((==) 2 . length . snd) $ map (\r -> (r, normalise $ mapMaybe (makeBoundedRowInterval r 0 upper) rs)) [0 .. upper]

solve2 = findRowWithMultipleIntervals 4_000_000 . map toRange

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
