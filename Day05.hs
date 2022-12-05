{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day05 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.Time.Format.ISO8601 (intervalFormat)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, hspace1, newline, space1, string, upperChar)

type Input = ([[Char]], [(Int, Int, Int)])

interval = do
  i1 <- parseInt
  char '-'
  i2 <- parseInt
  pure (i1, i2)

intervals = do
  i1 <- interval
  char ','
  i2 <- interval
  pure (i1, i2)

parseCrate = c <|> s
  where
    c = do
      char '['
      n <- upperChar
      char ']'
      pure (Just n)
    s = string "   " $> Nothing

parseStackRow = sepBy1 parseCrate (char ' ')

parseStacks = stripStacks <$> sepByNewline parseStackRow

stripStacks = map catMaybes . transpose

parseMove = do
  string "move "
  n <- parseInt
  string " from "
  from <- parseInt
  string " to "
  to <- parseInt
  pure (n, from, to)

parseMoves = sepByNewline parseMove

parseInput :: Parser Input
parseInput = do
  stacks <- parseStacks
  newline
  ns <- sepBy1 (char ' ' *> parseInt <* char ' ') (char ' ')
  newline
  newline
  moves <- parseMoves
  pure (stacks, moves)

forceMaybe (Just x) = x
forceMaybe Nothing = error "no value"

makeMove9000 css (0, from, to) = css
makeMove9000 css (n, from, to) = makeMove9000 (makeMove9001 css (1, from, to)) (n - 1, from, to)

solve1 = mapMaybe (viaNonEmpty head) . uncurry (foldl' makeMove9000)

makeMove9001 css (n, from, to) = [makeStack i | i <- [1 .. length css]]
  where
    (toHoist, newFrom) = splitAt n $ forceMaybe $ css !!? (from - 1)
    newTo = toHoist ++ forceMaybe (css !!? (to - 1))
    makeStack i
      | i == from = newFrom
      | i == to = newTo
      | otherwise = forceMaybe $ css !!? (i - 1)

solve2 = mapMaybe (viaNonEmpty head) . uncurry (foldl' makeMove9001)

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
