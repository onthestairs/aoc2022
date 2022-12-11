{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', last', parseInt, sepByNewline, tail')
import Control.Lens ((%~), (^.), _Just)
import Control.Lens.At (At (..))
import Control.Lens.Operators ((?~))
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Relude hiding (Op)
import Text.Megaparsec (sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Debug (dbg)

data Var = Old | Lit Int deriving (Show)

data Op = Add | Mul deriving (Show)

data Operation = Operation Var Op Var deriving (Show)

data Monkey = Monkey
  { items :: [Int],
    operation :: Operation,
    test :: (Int, Int, Int),
    inspections :: Int
  }
  deriving (Show, Generic)

type Input = Map.Map Int Monkey

parseVar = string "old" $> Old <|> Lit <$> parseInt

parseOp = char '*' $> Mul <|> char '+' $> Add

parseOperation = do
  v1 <- parseVar
  char ' '
  op <- parseOp
  char ' '
  v2 <- parseVar
  pure $ Operation v1 op v2

parseMonkey = do
  string "Monkey "
  monkeyId <- parseInt
  char ':'
  newline
  string "  Starting items: "
  items <- sepBy parseInt (string ", ")
  newline
  string "  Operation: new = "
  operation <- parseOperation
  newline
  string "  Test: divisible by "
  d <- parseInt
  newline
  string "    If true: throw to monkey "
  m1 <- parseInt
  newline
  string "    If false: throw to monkey "
  m2 <- parseInt
  newline
  pure $
    ( monkeyId,
      Monkey
        { items = items,
          operation = operation,
          test = (d, m1, m2),
          inspections = 0
        }
    )

parseInput :: Parser Input
parseInput = Map.fromList <$> sepByNewline parseMonkey

extractItem :: Monkey -> (Maybe Int, Monkey)
extractItem m = case m ^. #items of
  [] -> (Nothing, m)
  (i : is) -> (Just i, m & #items %~ tail')

div3 :: Int -> Int
div3 n = n `div` 3

toVal (Lit n) _ = n
toVal Old o = o

toF Mul = (*)
toF Add = (+)

runOp (Operation v1 op v2) n = f n1 n2
  where
    n1 = toVal v1 n
    n2 = toVal v2 n
    f = toF op

runTest (d, m1, m2) n = if n `mod` d == 0 then m1 else m2

runItem :: Monkey -> Int -> (Int, Int)
runItem m item = (i, w3)
  where
    w1 = item
    w2 = runOp (m ^. #operation) w1
    w3 = div3 w2
    i = runTest (m ^. #test) w3

doMonkey :: Map Int Monkey -> Int -> Map Int Monkey
doMonkey ms i = ms''
  where
    m = forceMaybe $ ms ^. at i
    (maybeItem, m') = extractItem m
    ms' = ms & at i ?~ m'
    go Nothing = ms'
    go (Just item) = doMonkey ns i
      where
        (nextMonkey, nextItem) = runItem m item
        ns =
          ms'
            & (at nextMonkey . _Just . #items) %~ (\items -> items ++ [nextItem])
            & (at i . _Just . #inspections) %~ (+ 1)
    ms'' = go maybeItem

doRound :: Map Int Monkey -> Map Int Monkey
doRound ms = foldl' doMonkey ms [0 .. 7]

solve1 = product . take 2 . reverse . sort . map (\m -> m ^. #inspections) . Map.elems . head' . drop 20 . iterate doRound

solve2 _ = Just 2

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
