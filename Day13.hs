{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.List (elemIndex, findIndex)
import Relude
import Text.Megaparsec (between, sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Packet = I Int | L [Packet] deriving (Show, Eq)

type Input = [(Packet, Packet)]

parseI = I <$> parseInt

parseL = L <$> between (char '[') (char ']') (sepBy parsePacket (char ','))

parsePacket = parseL <|> parseI

parsePacketPair = do
  p1 <- parsePacket
  newline
  p2 <- parsePacket
  newline
  pure (p1, p2)

parseInput :: Parser Input
parseInput = sepByNewline parsePacketPair

data Order = Yes | No | NotSure deriving (Eq, Show)

isInRightOrder (I n1) (I n2)
  | n1 < n2 = Yes
  | n2 < n1 = No
  | otherwise = NotSure
isInRightOrder (L p1s) (L p2s) = go p1s p2s
  where
    go (p1 : ps1') (p2 : ps2') = if c == NotSure then go ps1' ps2' else c
      where
        c = isInRightOrder p1 p2
    go [] (p2 : ps2') = Yes
    go (p1 : ps1') [] = No
    go [] [] = NotSure
isInRightOrder (I n1) (L ps2) = isInRightOrder (L [I n1]) (L ps2)
isInRightOrder (L ps1) (I n2) = isInRightOrder (L ps1) (L [I n2])

isInRightOrder' p1 p2 = (== Yes) $ isInRightOrder p1 p2

solve1 = sum . map fst . filter snd . zip [1 ..] . map (uncurry isInRightOrder')

toL (p1, p2) = [p1, p2]

extra1 = L [L [I 2]]

extra2 = L [L [I 6]]

extras = [extra1, extra2]

findIndexes ks xs = map (+ 1) $ mapMaybe (`elemIndex` xs) ks

solve2 = product . findIndexes extras . sortBy sorter . (extras ++) . concatMap toL
  where
    sorter p1 p2 = if isInRightOrder' p1 p2 then LT else GT

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
