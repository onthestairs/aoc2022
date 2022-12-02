{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day01 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [[Int]]

parseInts :: Parser Input
parseInts = sepBy1 (sepByNewline parseInt) "\n\n"

solve1 = viaNonEmpty maximum1 . map sum

solve2 = sum . take 3 . reverse . sort . map sum

solution =
  Solution
    { _parse = parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
