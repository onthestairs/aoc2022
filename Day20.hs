{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day20 (solution) where

import AOC (Parser, Solution (..), forceMaybe, parseInt, parseNegativeInt, sepByNewline)
import Control.Monad.ST (runST)
import Data.List (nub)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (lowerChar, newline)

type Input = [Int]

parseInput :: Parser Input
parseInput = sepByNewline (parseInt <|> parseNegativeInt)

makeSwapsRight i 0 l = []
makeSwapsRight i n l = (i', j') : makeSwapsRight j' (n - 1) l
  where
    (i', j') = if i == l - 1 then (i, 0) else (i, i + 1)

makeSwapsLeft i 0 l = []
makeSwapsLeft i n l = (i', j') : makeSwapsLeft j' (n - 1) l
  where
    (i', j') = if i == 0 then (i, l - 1) else (i, i - 1)

makeSwaps i n l = if n > 0 then makeSwapsRight i n l else makeSwapsLeft i (-n) l

move ns i n = forM_ swaps (\(i, j) -> MV.swap ns i j)
  where
    swaps = makeSwaps i n (MV.length ns)

wrappedLookup xs i = (V.!) xs i'
  where
    i' = i `mod` V.length xs

mix ns' = forM [0 .. MV.length ns' - 1] $ \originalIndex -> do
  ns'' <- V.freeze ns'
  let index = traceShow originalIndex $ forceMaybe $ V.findIndex ((==) originalIndex . fst) ns''
  let (_, n) = (V.!) ns'' index
  move ns' index n

extractResult v = sum $ map (snd . wrappedLookup v . (+ zeroIndex)) indexes
  where
    indexes = [1000, 2000, 3000]
    zeroIndex = forceMaybe $ V.findIndex ((==) 0 . snd) v

solve1 ns = runST $ do
  let ins = V.indexed $ V.fromList ns
  ns' <- V.thaw ins
  mix ns'
  finalNs <- V.freeze ns'
  pure $ extractResult finalNs

solve2 = const 2

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
