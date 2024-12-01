module Day01Pt1 where

import Data.List

day01Pt1 = do
  contents <- readFile "data/day1-input.txt"
  let linesOfFile = lines contents
  -- for each line, create tuples
  let tuples = strToTuple linesOfFile
  -- split into separate arrays
  let arrs = tupleF toInt $ unzip tuples
  -- sort each array
  let sorted = tupleF sort arrs
  -- get difference between each number in both arrays
  let allDiffs = diffs sorted
  -- sum differences
  let result = sum allDiffs
  print result

diffs :: ([Int], [Int]) -> [Int]
diffs ([], []) = []
diffs (_, []) = []
diffs ([], _) = []
diffs (x:xs, y:ys) = abs (x - y) : diffs(xs, ys)

tupleF :: ([a] -> [b]) -> ([a], [a]) -> ([b], [b])
tupleF f (a, b) = (f a, f b)

toInt :: [String] -> [Int]
toInt = map read

strToTuple :: [String] -> [(String, String)]
strToTuple = map toTuple

toTuple :: String -> (String, String)
toTuple a = joinWords $ words a

joinWords :: [x] -> (x, x)
joinWords [a, b] = (a, b)
joinWords _ = error "bad input"