{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day16 (solution) where

import AOC (Parser, Solution (..), forceMaybe, head', parseInt, sepByNewline, tail')
import qualified Data.Graph.Inductive as Graph
import Data.List (elemIndex, findIndex, maximum, maximumBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import Relude
import Text.Megaparsec (between, sepBy, sepBy1)
import Text.Megaparsec.Char (char, newline, string, upperChar)

type Input = Map.Map String (Int, [String])

parseValveName = many upperChar

parseValve = do
  string "Valve "
  valve <- parseValveName
  string " has flow rate="
  rate <- parseInt
  (string "; tunnels lead to valves ") <|> (string "; tunnel leads to valve ")
  links <- sepBy1 parseValveName (string ", ")
  pure (valve, (rate, links))

parseInput :: Parser Input
parseInput = Map.fromList <$> sepByNewline parseValve

-- findBestPath start totalMinutes m = go (0, Set.empty) start 0
--   where
--     goodValves = traceShowId $ Map.keysSet $ Map.filter (> 0) $ Map.map fst m
--     go :: (Int, Set.Set String) -> String -> Int -> Int
--     go _ _ minutes | (minutes + 1) == totalMinutes = 0
--     go (score, open) _ minutes | open == goodValves = (totalMinutes - minutes) * score
--     go (score, open) valve minutes = score + maxNext
--       where
--         currentScore = fst $ fromJust $ Map.lookup valve m
--         maxNext =
--           if (not $ Set.member valve open) && (Set.member valve goodValves)
--             then go (score + currentScore, Set.insert valve open) valve (minutes + 1)
--             else maximum $ [go (score, open) v' (minutes + 1) | v' <- snd $ fromJust $ Map.lookup valve m, v' ]
--

-- stateTree :: Int -> M.Map Node (M.Map Node Int) -> Node -> M.Map Node Int -> T.Tree (Node, Int)
-- stateTree maxT dists start valves = T.unfoldTree buildNode (1, start, M.empty)
--   where
--     nextStates (t, v, zs) =
--       if
--           | t >= maxT ->
--               []
--           | v /= start && v `M.notMember` zs ->
--               [(t + 1, v, M.insert v ((maxT - t) * valves M.! v) zs)]
--           | otherwise ->
--               [(t + dists M.! v M.! w, w, zs) | w <- M.keys valves, w `M.notMember` zs]
--     buildNode (t, v, zs) =
--       let succs = nextStates (t, v, zs)
--        in ((v, if null succs then foldl' (+) 0 zs else 0), succs)

getPressure m valve = fst $ fromJust $ Map.lookup valve m

type Valve = Int

getAdjacent :: Map.Map Valve (Int, [(Int, Valve)]) -> Valve -> [(Int, Valve)]
getAdjacent m valve = map (\(d, p) -> (d, p)) $ snd $ fromJust $ Map.lookup valve m

buildPathsTree :: Int -> Valve -> Map.Map Valve (Int, [(Int, Valve)]) -> Tree.Tree (Valve, Int)
buildPathsTree totalMinutes start m = Tree.unfoldTree makeNode (1, start, 0, Set.empty)
  where
    makeNext minutes valve total open
      | minutes > totalMinutes = []
      | valve /= start && Set.notMember valve open = [(minutes + 1, valve, total + ((totalMinutes - minutes) * getPressure m valve), Set.insert valve open)]
      | otherwise = [(minutes + d, nextValve, total, open) | (d, nextValve) <- getAdjacent m valve]
    -- makeNode (time, valve, total, open) = ((valve, if null next then total else 0), next)
    makeNode (time, valve, total, open) = ((valve, if null next then total else 0), next)
      where
        next = makeNext time valve total open

traceW f = uncurry trace . (f &&& id)

-- makeGoodSubmap :: Map.Map Valve (Int, [Valve]) -> Map.Map Valve (Int, [(Int, Valve)])
makeGoodSubmap start m = Map.mapWithKey (\valve (p, _) -> (p, paths valve)) $ Map.filterWithKey (\valve _ -> Set.member valve goodValves || valve == start) m
  where
    goodValves = Map.keysSet $ Map.filter (> 0) $ Map.map fst m
    nodes = map (\v -> (v, iToV v)) $ Map.keys m
    edges :: [(Valve, Valve, Int)]
    edges = concat $ Map.elems $ Map.mapWithKey (\k (_, as) -> map (\a -> (k, a, 1)) as) m
    graph :: Graph.Gr String Int
    graph = Graph.mkGraph nodes edges
    paths valve = mapMaybe f $ Graph.bft valve graph
      where
        f path = if Set.member l goodValves && (l /= valve) then Just (length path - 1, l) else Nothing
          where
            l = head' path

vToI [c1, c2] = ord c1 * 100 + ord c2

iToV n = [chr (n `div` 100), chr (n `mod` 100)]

leaves :: Tree.Tree (a, b) -> [([a], b)]
leaves (Tree.Node (node, v) []) = [([node], v)]
leaves (Tree.Node (node, _) cs) = map (\(ns, v) -> (node : ns, v)) $ concatMap leaves cs

toIs = Map.fromList . map (\(k, (p, ns)) -> (vToI k, (p, map vToI ns))) . Map.toList

toVs = Map.fromList . map (\(k, (p, ns)) -> (iToV k, (p, map (\(d, n) -> (d, iToV n)) ns))) . Map.toList

solve1 :: Input -> Int
-- solve1 = maximum . map snd . leaves . traceW (Tree.drawTree . fmap (show . first iToV)) . buildPathsTree 15 start . (uncurry traceShow) . (toVs &&& id) . makeGoodSubmap start . toIs
solve1 = maximum . map snd . leaves . buildPathsTree 30 start . makeGoodSubmap start . toIs
  where
    start = vToI "AA"

keepValves vs = Map.map (second (filter (\(d, v) -> Set.member v vs))) . flip Map.restrictKeys vs

makeSplits :: [a] -> [([a], [a])]
makeSplits [] = [([], [])]
makeSplits (x : xs) = concat [[(x : as, bs), (as, x : bs)] | (as, bs) <- makeSplits xs]

getMaxPressure start n m = snd $ maximumBy (comparing snd) $ leaves $ buildPathsTree n start m

solve2 i = maximum ps
  where
    start = vToI "AA"
    sm = makeGoodSubmap start $ toIs i
    vs = Map.keys sm
    splits :: [([Valve], [Valve])]
    splits = makeSplits vs
    ps = map pressure splits
    pressure (vs1, vs2) = getMaxPressure start 26 m1 + getMaxPressure start 26 m2
      where
        m1 = keepValves (Set.fromList (start : vs1)) sm
        m2 = keepValves (Set.fromList (start : vs2)) sm

-- (path1, pressure1) =
--     -- sm' = removeValves (Set.fromList (traceW (show . map iToV) path1)) sm
--     -- keep the first path (ie. the starting path)
--     sm' = removeValves (Set.fromList (tail' path1)) sm
--     (path2, pressure2) = maximumBy (comparing snd) $ leaves $ buildPathsTree 26 start sm'
-- solve2 = const 2

solution =
  Solution
    { _parse = parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
