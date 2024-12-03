module Day03Pt2 where

import Data.Char

day03Pt2 :: IO ()
day03Pt2 = do
  contents <- readFile "data/day3-input.txt"
  let line = concat $ lines contents
  let result = mulAll (getNumberPairs line)
  print result

evalZero :: (Int, Int)
evalZero = (1, 0)

-- process line and get all number pairs to multiply and sum
getNumberPairs :: [Char] -> [(Int,Int)]
getNumberPairs [] = []
getNumberPairs x = processMultipleMulsSingle True x

-- sum the pair's multiplication result
mulAll :: [(Int,Int)] -> Int
mulAll = foldr ((+) . uncurry (*)) 0

-- get all pairs of numbers from a line
processMultipleMulsSingle :: Bool -> [Char] -> [(Int,Int)]
processMultipleMulsSingle _ [] = []
processMultipleMulsSingle b x | changedRange b x = processMultipleMulsSingle (not b) (drop 1 x)
                              | b = hasValidMulString x : processMultipleMulsSingle b (drop 1 x)
                              | otherwise = processMultipleMulsSingle b (drop 1 x)

-- check if we have changed whether we should count the muls
changedRange :: Bool -> [Char] -> Bool
changedRange b ('d':'o':'n':'\'':'t':'(':')':_) | b = True
                                                | otherwise = False
changedRange b ('d':'o':'(':')':_) | not b = True 
                                    | otherwise = False
changedRange _ _ = False

-- get a pair of numbers from a line
hasValidMulString :: [Char] -> (Int, Int)
hasValidMulString ('m':'u':'l':cs) = hasOpenParen cs
hasValidMulString _ = evalZero

-- is the next char an open paranthesis? if so, continue the check
hasOpenParen :: [Char] -> (Int, Int)
hasOpenParen ('(':cs) = isNumSeq cs hasComma
hasOpenParen _ = evalZero

-- is the next grouping of chars a number? if so, continue the check
isNumSeq :: [Char] -> ([Char] -> Int -> (Int, Int)) -> (Int, Int)
isNumSeq cs f | not (null n) = f (dropWhile isDigit cs) (read n :: Int)
              | otherwise = evalZero
            where n = takeWhile isDigit cs

-- is the next char a comma? if so, continue the check
hasComma :: [Char] -> Int -> (Int, Int)
hasComma (',':cs) n = isNumSeq cs (hasClosingParen n)
hasComma _ _ = evalZero

-- is the next char an closing paranthesis? if so, we have finished and found valid pair of numbers to return
hasClosingParen :: Int -> [Char] -> Int -> (Int, Int)
hasClosingParen n1 (')':_) n2  = (n1, n2)
hasClosingParen _ _ _ = evalZero
