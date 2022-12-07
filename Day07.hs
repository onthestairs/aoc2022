{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 (solution) where

import AOC (Parser, Solution (..), parseInt, sepByNewline)
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable (Recursive (cata), ana, hylo)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (nub)
import Data.Tree (Tree (..), drawTree, flatten, unfoldTree)
import Relude
import Relude.Extra (Foldable1 (maximum1, minimum1))
import Text.Megaparsec (eof, noneOf, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline, string)

data Object = Dir String | File String Int deriving (Show)

data Output = CdUp | Cd String | Ls [Object] deriving (Show)

type Input = [Output]

cd = do
  string "$ cd "
  dir <- many (noneOf ['\n'])
  if dir == ".." then pure CdUp else pure (Cd dir)

ls = do
  string "$ ls"
  char '\n'
  os <- sepByNewline (dir <|> file)
  pure $ Ls os

dir = do
  string "dir "
  dir <- many (noneOf ['\n'])
  pure (Dir dir)

file = do
  size <- parseInt
  char ' '
  name <- many (noneOf ['\n'])
  pure (File name size)

output = cd <|> ls

parseInput :: Parser Input
parseInput = sepByNewline output

data FS a = FSDir a [FS a] | FSFile (String, Int) deriving (Show)

makeBaseFunctor ''FS

init' = reverse . drop 1 . reverse

forceMaybe (Just x) = x
forceMaybe Nothing = error "no value"

last' = forceMaybe . viaNonEmpty last

makeFileList :: [Output] -> [([String], Int)]
makeFileList = go []
  where
    go _ [] = []
    go cwd (CdUp : cs') = go (init' cwd) cs'
    go cwd ((Cd d) : cs') = go (cwd ++ [d]) cs'
    go cwd (Ls os : cs') =
      mapMaybe makeFile os ++ go cwd cs'
      where
        makeFile (Dir name) = Nothing
        makeFile (File name size) = Just (cwd ++ [name], size)

isListPrefix prefix xs = take (length prefix) xs == prefix

nextPrefixes :: [String] -> [([String], Int)] -> [[String]]
nextPrefixes prefix xs = nub $ mapMaybe f xs
  where
    f (path, size)
      | path == prefix = Nothing
      | isListPrefix prefix path = Just (take (length prefix + 1) path)
      | otherwise = Nothing

makeFileTree :: [([String], Int)] -> FS String
makeFileTree cs = ana coalg ["/"]
  where
    coalg prefix = case find ((==) prefix . fst) cs of
      (Just (file, size)) -> FSFileF (last' file, size)
      Nothing -> FSDirF (last' prefix) (nextPrefixes prefix cs)

annotateSubSizesAlg :: FSF String (FS (String, Int)) -> FS (String, Int)
annotateSubSizesAlg (FSDirF name cs) = FSDir (name, sum $ map getSize cs) cs
  where
    getSize (FSDir (name, size) _) = size
    getSize (FSFile (name, size)) = size
annotateSubSizesAlg (FSFileF (name, size)) = FSFile (name, size)

annotateSubSizes :: FS String -> FS (String, Int)
annotateSubSizes = cata annotateSubSizesAlg

countSmallDirsAlg :: Int -> FSF (String, Int) Int -> Int
countSmallDirsAlg n (FSDirF (name, size) cs) = if size <= n then size + sum cs else sum cs
countSmallDirsAlg n _ = 0

findSmallDirs :: Int -> FS String -> Int
findSmallDirs n = cata (countSmallDirsAlg n) . cata annotateSubSizesAlg

solve1 = findSmallDirs 100_000 . makeFileTree . makeFileList

possibleDirsToDelete n (FSDirF (name, size) cs) = if size >= n then size : concat cs else concat cs
possibleDirsToDelete n _ = []

solve2 cs = viaNonEmpty minimum1 $ cata (possibleDirsToDelete minimumSize) fs
  where
    fs = cata annotateSubSizesAlg $ makeFileTree $ makeFileList cs
    totalSpace = case fs of FSDir ("/", size) _ -> size
    minimumSize = 30_000_000 - (70_000_000 - totalSpace)

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
