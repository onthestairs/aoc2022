{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Advent (AoC (AoCInput, AoCSubmit), defaultAoCOpts, runAoC)
-- import Day14
-- import Day15
-- import Day16
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
      (13, SimpleSolution Day13.solution)
      -- (14, SimpleSolution Day14.solution),
      -- (15, SimpleSolution Day15.solution),
      -- (16, SimpleSolution Day16.solution),
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

toAoCPart Part1 = AOC.Part1
toAoCPart Part2 = AOC.Part2

submitToAoC day part answer = do
  aocOpts <- getOpts
  let aocPart = toAoCPart part
  runAoC aocOpts $ AoCSubmit (mkDay_ (fromIntegral day)) aocPart (toString answer)

parseInput parser input = rightToMaybe $ runParser parser "" input

parseInputDebug parser input = runParser (dbg "input" parser) "" input

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

getSolution :: GenericSolution -> Int -> Part -> IO (Either Text Text)
getSolution (SimpleSolution solution) day part = do
  input <- getInput day
  let maybeParsed = parseInput (view #_parse solution) input
  case maybeParsed of
    Nothing -> pure $ Left "Couldn't parse"
    Just parsed -> do
      let part1Result = view #_solve1 solution $ parsed
      let part2Result = view #_solve2 solution $ parsed
      case part of
        Part1 -> pure $ Right $ show part1Result
        Part2 -> pure $ Right $ show part2Result
getSolution (TwoParseSolution solution) day part = do
  input <- getInput day
  case part of
    Part1 -> do
      let maybeParsed1 = parseInput (view #_parse1 solution) input
      case maybeParsed1 of
        Nothing -> pure $ Left $ "Couldn't parse part i"
        Just parsed1 -> do
          pure $ Right $ show $ (view #_solveWith1 solution) parsed1
    Part2 -> do
      let maybeParsed2 = parseInput (view #_parse2 solution) input
      case maybeParsed2 of
        Nothing -> pure $ Left $ "Couldn't parse part ii"
        Just parsed2 -> do
          pure $ Right $ show $ (view #_solveWith2 solution) parsed2

showSolution :: GenericSolution -> Int -> Part -> IO ()
showSolution solution day part = do
  result <- getSolution solution day part
  case result of
    Left err -> putTextLn err
    Right answer -> putTextLn answer

submitSolution :: GenericSolution -> Int -> Part -> IO ()
submitSolution solution day part = do
  result <- getSolution solution day part
  case result of
    Left err -> putTextLn err
    Right answer -> do
      aocResult <- submitToAoC day part answer
      putTextLn $ show $ aocResult

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

submit :: Int -> Part -> IO ()
submit day part = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just solution -> submitSolution solution day part
