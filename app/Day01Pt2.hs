module Day01Pt2 where

import Data.List

day01Pt2 = do
  contents <- readFile "data/day1-input.txt"
  let linesOfFile = lines contents
  -- for each line, create tuples
  let tuples = strToTuple linesOfFile
  -- split into separate arrays
  let arrs = tupleF toInt $ unzip tuples
  -- for each number in the first array, get similarity score
  let scores = similarityScore arrs
  -- sum similarity scores
  let total = sum scores
  print total

similarityScore :: ([Int], [Int]) -> [Int]
similarityScore ([], []) = []
similarityScore ([], y) = []
similarityScore (x:xs, []) = []
similarityScore (x:xs, y) = (x * occurence x y) : similarityScore (xs, y)

occurence :: Int -> [Int] -> Int
occurence _ [] = 0
occurence n (x:xs) = isEq n x + occurence n xs

isEq :: Int -> Int -> Int
isEq a b | a == b = 1
          | otherwise = 0

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