{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), SeparateParseSolution (..), GenericSolution (..), NoPuzzle (NoPuzzle), AOCShow, showResult, printResult, Parser, parseInt, parseDigit, parseInt64, parseInteger, parseNegativeInt, parseSignedInt, sepByNewline, init', head', tail', last', forceMaybe, chunk) where

import Control.Lens
import Relude
import Text.Megaparsec (Parsec, anySingle, lookAhead, runParser, try)
import Text.Megaparsec.Char (char, digitChar, newline)
import qualified Text.Megaparsec.Char.Lexer as L

data Solution a b c = Solution
  { _parse :: Parser a,
    _solve1 :: a -> b,
    _solve2 :: a -> c
  }
  deriving (Generic)

data SeparateParseSolution a b c d = SeparateParseSolution
  { _parse1 :: Parser a,
    _solveWith1 :: a -> b,
    _parse2 :: Parser c,
    _solveWith2 :: c -> d
  }
  deriving (Generic)

data NoPuzzle = NoPuzzle

class AOCShow a where
  showResult :: a -> Text
  printResult :: a -> IO ()
  printResult = putTextLn . showResult

instance Show a => AOCShow (Maybe a) where
  showResult (Just a) = show a
  showResult Nothing = "No result"

instance AOCShow Integer where
  showResult = show

instance AOCShow Text where
  showResult = id

instance AOCShow Int where
  showResult = show

instance Show a => AOCShow [a] where
  showResult = show

instance (Show a, Show b) => AOCShow (a, b) where
  showResult = show

instance AOCShow NoPuzzle where
  showResult = const ("No puzzle for this part" :: Text)

data GenericSolution where
  SimpleSolution :: (Show a, AOCShow b, AOCShow c) => Solution a b c -> GenericSolution
  TwoParseSolution :: (Show a, AOCShow b, Show c, AOCShow d) => SeparateParseSolution a b c d -> GenericSolution

type Path = String

type Parser = Parsec Void Text

readDigit '0' = Just 0
readDigit '1' = Just 1
readDigit '2' = Just 2
readDigit '3' = Just 3
readDigit '4' = Just 4
readDigit '5' = Just 5
readDigit '6' = Just 6
readDigit '7' = Just 7
readDigit '8' = Just 8
readDigit '9' = Just 9

parseDigit :: Parser Int
parseDigit = do
  c <- digitChar
  case readDigit c of
    Just n -> pure n
    Nothing -> fail "not a digit"

parseInt :: Parser Int
parseInt = L.decimal

parseInt64 :: Parser Int64
parseInt64 = L.decimal

parseInteger :: Parser Integer
parseInteger = L.decimal

parseSignedInt :: Parser Int
parseSignedInt = do
  sign <- char '+' <|> char '-'
  n <- parseInt
  let coefficient = if sign == '+' then 1 else -1
  pure $ coefficient * n

parseNegativeInt :: Parser Int
parseNegativeInt = do
  char '-'
  n <- parseInt
  pure $ (-1) * n

sepByNewline :: Parser a -> Parser [a]
sepByNewline p = do
  x <- p
  xs <- many $
    try $ do
      newline
      c <- lookAhead anySingle
      when (c == '\n') (fail "double newline")
      p
  pure $ x : xs

init' = reverse . drop 1 . reverse

forceMaybe (Just x) = x
forceMaybe Nothing = error "no value"

last' = forceMaybe . viaNonEmpty last

head' = forceMaybe . viaNonEmpty head

tail' = forceMaybe . viaNonEmpty tail

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)
