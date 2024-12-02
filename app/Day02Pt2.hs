module Day02Pt2 where

day02Pt2 :: IO ()
day02Pt2 = do
  contents <- readFile "data/day2-input.txt"
  let linesOfFile = lines contents
  let lineSplits = map stringToIntArr linesOfFile
  let safeCount = countSafe lineSplits
  print safeCount

-- convert string to int array
stringToIntArr :: String -> [Int]
stringToIntArr x = map read $ words x :: [Int]

-- count number of safe lines
countSafe :: [[Int]] -> Int
countSafe [] = 0
countSafe (x:xs) | isSafe x = 1 + countSafe xs
                  | otherwise = countSafe xs

-- check if a line is safe
isSafe :: [Int] -> Bool
isSafe x = isAsc x || isDesc x

-- check if numbers are ascending
isAsc :: [Int] -> Bool
isAsc = checkRule (\ x1 x2 -> x2 > x1 && x2 - x1 >= 1 && x2 - x1 <= 3) False (-1)

-- check if numbers are descending
isDesc :: [Int] -> Bool
isDesc = checkRule (\ x1 x2 -> x2 < x1 && x1 - x2 >= 1 && x1 - x2 <= 3) False (-1)

-- apply direction check to line
-- args: direction function, whether a level has been dropped, previous value, line numbers
checkRule :: (Int -> Int -> Bool) -> Bool -> Int -> [Int] -> Bool
checkRule _ _ _ [] = True
checkRule _ _ _ [_] = True
checkRule f b p (x1:x2:xs) | f x1 x2 = checkRule f b x1 (x2 : xs) -- pass, drop first operand and keep as previous
                            | not b && p == -1 = checkRule f True 0 (x1 : xs) || checkRule f True 0 (x2 : xs) -- fail, haven't dropped a level yet and no previous, try two branches by dropping both operands separately, record that a level has been dropped
                            | not b = checkRule f True 0 (x1 : xs) || checkRule f True 0 (p : x2 : xs) -- fail, haven't dropped a level yet and have a previous, try two branches by dropping both operands separately, record that a level has been dropped
                            | otherwise = False -- fail, have alread dropped a level