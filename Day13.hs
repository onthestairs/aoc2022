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

instance Ord Packet where
  compare (I n1) (I n2) = compare n1 n2
  compare (L p1s) (L p2s) = go p1s p2s
    where
      go [] (p2 : ps2') = LT
      go (p1 : ps1') [] = GT
      go [] [] = EQ
      go ps1 ps2 = compare ps1 ps2
  compare (I n1) (L ps2) = compare (L [I n1]) (L ps2)
  compare (L ps1) (I n2) = compare (L ps1) (L [I n2])

solve1 = sum . map fst . filter ((== LT) . snd) . zip [1 ..] . map (uncurry compare)

toL (p1, p2) = [p1, p2]

extra1 = L [L [I 2]]

extra2 = L [L [I 6]]

extras = [extra1, extra2]

findIndexes ks xs = map (+ 1) $ mapMaybe (`elemIndex` xs) ks

solve2 = product . findIndexes extras . sort . (extras ++) . concatMap toL

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
