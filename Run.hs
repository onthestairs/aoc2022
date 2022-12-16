{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Advent (AoC (AoCInput, AoCSubmit), defaultAoCOpts, runAoC)
-- import Day17
-- import Day18
-- import Day20
-- import Day21
-- import Day22

import qualified Advent as AOC
import Advent.Types (mkDay_)
import Control.Lens
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Relude
import System.Environment (getEnv)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Debug (dbg)

solutions =
  Map.fromList
    [ (1, SimpleSolution Day01.solution),
      (2, SimpleSolution Day02.solution),
      (3, SimpleSolution Day03.solution),
      (4, SimpleSolution Day04.solution),
      (5, SimpleSolution Day05.solution),
      (6, SimpleSolution Day06.solution),
      (7, SimpleSolution Day07.solution),
      (8, SimpleSolution Day08.solution),
      (9, SimpleSolution Day09.solution),
      (10, SimpleSolution Day10.solution),
      (11, SimpleSolution Day11.solution),
      (12, SimpleSolution Day12.solution),
      (13, SimpleSolution Day13.solution),
      (14, SimpleSolution Day14.solution),
      (15, SimpleSolution Day15.solution),
      (16, SimpleSolution Day16.solution)
      -- (17, SimpleSolution Day17.solution),
      -- (18, SimpleSolution Day18.solution),
      -- (20, SimpleSolution Day20.solution),
      -- (21, SimpleSolution Day21.solution),
      -- (22, SimpleSolution Day22.solution)
    ]

data Part = Part1 | Part2 | Both deriving (Eq)

getOpts = do
  key <- getEnv "ADVENT_OF_CODE_TOKEN"
  pure $ defaultAoCOpts 2022 key

getInput :: Int -> IO Text
getInput day = do
  aocOpts <- getOpts
  result <- runAoC aocOpts $ AoCInput (mkDay_ (fromIntegral day))
  case result of
    (Left _) -> error "no input"
    (Right input) -> pure input

parseInput parser input = rightToMaybe $ runParser parser "" input

parseInputDebug parser = runParser (dbg "input" parser) ""

peekInput :: Int -> IO ()
peekInput day = do
  let maybeSolution = Map.lookup day solutions
  input <- getInput day
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just (SimpleSolution solution) -> do
      let parsed = parseInputDebug (view #_parse solution) input
      print parsed
    Just (TwoParseSolution solution) -> do
      let parsed1 = parseInputDebug (view #_parse1 solution) input
      putStrLn "Part 1"
      print parsed1
      let parsed2 = parseInputDebug (view #_parse2 solution) input
      putStrLn "Part 2"
      print parsed2

showSolution :: GenericSolution -> Int -> Part -> IO ()
showSolution (SimpleSolution solution) day part = do
  input <- getInput day
  let maybeParsed = parseInput (view #_parse solution) input
  case maybeParsed of
    Nothing -> putStrLn "Couldn't parse"
    Just parsed -> do
      let part1Result = view #_solve1 solution parsed
      let part2Result = view #_solve2 solution parsed
      case part of
        Part1 -> printResult part1Result
        Part2 -> printResult part2Result
        Both -> do
          printResult part1Result
          printResult part2Result
showSolution (TwoParseSolution solution) day part = do
  input <- getInput day
  when (part == Part1 || part == Both) $ do
    let maybeParsed1 = parseInput (view #_parse1 solution) input
    case maybeParsed1 of
      Nothing -> putStrLn "Couldn't parse part i"
      Just parsed1 -> do
        printResult $ view #_solveWith1 solution parsed1
  when (part == Part2 || part == Both) $ do
    let maybeParsed2 = parseInput (view #_parse2 solution) input
    case maybeParsed2 of
      Nothing -> putStrLn "Couldn't parse part ii"
      Just parsed2 -> do
        printResult $ view #_solveWith2 solution parsed2

solve :: Int -> Part -> IO ()
solve day part = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just solution -> showSolution solution day part

solveAll :: IO ()
solveAll = do
  let solutionsList = Map.toList solutions
  for_ solutionsList \(day, solution) -> do
    putStrLn $ "Day " <> show day
    putStrLn "------"
    showSolution solution day Both
    putStrLn ""
