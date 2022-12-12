{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', last', parseInt, sepByNewline, tail')
import Control.Lens (Field1 (_1), view, (%~), (.~), (^.), _Just)
import Control.Lens.At (At (..))
import Control.Lens.Operators ((?~))
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Relude hiding (Op)
import Text.Megaparsec (sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

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

extractItems :: Map.Map Int Monkey -> Int -> ([Int], Map.Map Int Monkey)
extractItems ms i = case ms ^. (at i . _Just . #items) of
  items -> (items, ms & (at i . _Just . #items) .~ [])

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

runItem :: (Int -> Int) -> Monkey -> Int -> (Int, Int)
runItem f m item = (i, w3)
  where
    w1 = item
    w2 = runOp (m ^. #operation) w1
    w3 = f w2
    i = runTest (m ^. #test) w3

doMonkey f ms i = ms''
  where
    m = forceMaybe $ ms ^. at i
    (items, ms') = extractItems ms i
    go :: Map.Map Int Monkey -> Int -> Map.Map Int Monkey
    go ns item = ns'
      where
        (nextMonkey, nextItem) = runItem f m item
        ns' =
          ns
            & (at nextMonkey . _Just . #items) %~ (\items -> items ++ [nextItem])
            & (at i . _Just . #inspections) %~ (+ 1)
    ms'' = foldl' go ms' items

doRound f ms = foldl' (doMonkey f) ms [0 .. 7]

solve f n = product . take 2 . reverse . sort . map (\m -> m ^. #inspections) . Map.elems . head' . drop n . uncurry iterate . (doRound . f &&& id)

solve1 = solve f 20
  where
    f ms = div3

divisors = map (view (#test . _1)) . Map.elems

-- use the chinese remainder theorem to reduce n to something smaller which
-- keeps the mod values constant
makeModAll ms = \n -> n `mod` k
  where
    k = product $ divisors ms

solve2 = solve makeModAll 10000

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
