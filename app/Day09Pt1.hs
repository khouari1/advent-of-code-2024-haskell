module Day09Pt1 where

import Data.Char (digitToInt)

type Coord = (Int,Int)

day09Pt1 :: IO ()
day09Pt1 = do
  contents <- readFile "data/day9-input.txt"
  let line = head $ lines contents
  let pairs = getPairs line
  let numbers = getNumberOfIndicies pairs 0
  let reversed = reverse numbers
  let total = sum $ map fst pairs
  let result = checksum 0 $ take total $ process reversed pairs 0 total -- fixme: take is a hack
  print result

checksum :: Int -> [Int] -> Int
checksum _ [] = 0
checksum i (x:xs) = (i * x) + checksum (i + 1) xs

getPairs :: [Char] -> [(Int,Int)]
getPairs [] = []
getPairs [x] = [(digitToInt x,0)]
getPairs (x1:x2:xs) = (digitToInt x1,digitToInt x2) : getPairs xs

getNumberOfIndicies :: [(Int,Int)] -> Int -> [Int]
getNumberOfIndicies [] _ = []
getNumberOfIndicies ((x1,_):xs) n = replicate x1 n ++ getNumberOfIndicies xs (n + 1)

process :: [Int] -> [(Int,Int)] -> Int -> Int -> [Int]
process [] _ _ _ = []
process _ [] _ _ = []
process (n:ns) ((x,y):xs) i t | i > t = []
                              | y > 0 = r ++ [n] ++ process ns ((0,y-1) : xs) i t
                              | otherwise = r ++ process (n:ns) xs (i + 1) t
                              where r = replicate x i