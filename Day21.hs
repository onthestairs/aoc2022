{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day21 (solution) where

import AOC (AOCShow (showResult), Parser, Solution (..), forceMaybe, parseInt, sepByNewline)
import Data.Function.Memoize (memoFix)
import Data.List (nub)
import qualified Data.Map as Map
import Foreign.C (e2BIG)
import Relude hiding (Op)
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline, string)

data Op = Add | Sub | Mul | Div deriving (Eq, Show)

data Expr = Name String | Var String | Lit Int | F Op Expr Expr deriving (Eq, Show)

instance AOCShow Expr where
  showResult (Name n) = toText n
  showResult (Var n) = toText n
  showResult (Lit n) = show n
  showResult (F Add e1 e2) = "(" <> showResult e1 <> "+" <> showResult e2 <> ")"
  showResult (F Sub e1 e2) = "(" <> showResult e1 <> "-" <> showResult e2 <> ")"
  showResult (F Mul e1 e2) = "(" <> showResult e1 <> "*" <> showResult e2 <> ")"
  showResult (F Div e1 e2) = "(" <> showResult e1 <> "/" <> showResult e2 <> ")"

type Input = Map.Map String Expr

parseName = many lowerChar

parseLit = Lit <$> parseInt

parseOp = char '+' $> Add <|> char '-' $> Sub <|> char '*' $> Mul <|> char '/' $> Div

parseF = do
  n1 <- parseName
  char ' '
  op <- parseOp
  char ' '
  F op (Name n1) . Name <$> parseName

parseExpr = parseLit <|> parseF

parseExprLine = do
  name <- parseName
  string ": "
  expr <- parseExpr
  pure (name, expr)

parseInput :: Parser Input
parseInput = Map.fromList <$> sepByNewline parseExprLine

lookup m k = forceMaybe $ Map.lookup m k

evalOp Add (Lit n1) (Lit n2) = Lit $ n1 + n2
evalOp Sub (Lit n1) (Lit n2) = Lit $ n1 - n2
evalOp Mul (Lit n1) (Lit n2) = Lit $ n1 * n2
evalOp Div (Lit n1) (Lit n2) = Lit $ n1 `div` n2
evalOp o n1 n2 = F o n1 n2

eval :: String -> Input -> Expr
eval startName m = goMemo startName
  where
    goMemo = memoFix $ \go name -> case lookup name m of
      (F op (Name n1) (Name n2)) -> evalOp op (go n1) (go n2)
      e -> e

solve1 = eval "root"

data Equation = Equation Expr Expr deriving (Eq)

converge :: Eq a => (a -> a) -> a -> a
converge f x = if x == x' then x else converge f x'
  where
    x' = f x

simplify :: Equation -> Equation
simplify (Equation e1 e2) = uncurry Equation $ converge simplify' (e1, e2)
  where
    simplify' (F Mul (Lit n1) e1, Lit n2) = (e1, Lit $ n2 `div` n1)
    simplify' (F Add (Lit n1) e1, Lit n2) = (e1, Lit $ n2 - n1)
    simplify' (F Sub (Lit n1) e1, Lit n2) = (e1, Lit $ n1 - n2)
    simplify' (F Div (Lit n1) e1, Lit n2) = (e1, Lit $ n1 `div` n2)
    simplify' (F Mul e1 (Lit n1), Lit n2) = (e1, Lit $ n2 `div` n1)
    simplify' (F Add e1 (Lit n1), Lit n2) = (e1, Lit $ n2 - n1)
    simplify' (F Sub e1 (Lit n1), Lit n2) = (e1, Lit $ n2 + n1)
    simplify' (F Div e1 (Lit n1), Lit n2) = (e1, Lit $ n2 * n1)
    simplify' es = es

eval2 m = extract $ simplify $ Equation (goMemo rootN1) (goMemo rootN2)
  where
    m' = Map.insert "humn" (Var "humn") m
    (F Add (Name rootN1) (Name rootN2)) = lookup "root" m'
    goMemo = memoFix $ \go name -> case lookup name m' of
      (F op (Name n1) (Name n2)) -> evalOp op (go n1) (go n2)
      e -> e
    extract (Equation (Var "humn") n) = n

solve2 = eval2

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
