module Day07Pt1 where

import Data.Char
import Data.Maybe (catMaybes)

day07Pt1 :: IO ()
day07Pt1 = do
  contents <- readFile "data/day7-input.txt"
  let allLines = lines contents
  let allNumberLines = catMaybes $ getNumbers <$> allLines
  let results = map (\(a,xs) -> tryAnswer xs a 0) allNumberLines
  let result = sum results
  print result

getNumbers :: [Char] -> Maybe (Int,[Int])
getNumbers [] = Nothing
getNumbers xs = Just (nAnswer, numberList remainder [] [])
              where answer = takeWhile isDigit xs
                    nAnswer = read answer :: Int
                    remainder = drop (length answer) xs

numberList :: [Char] -> [Char] -> [Int] -> [Int]
numberList [] [] [] = []
numberList [] [] l = l
numberList [] acc l = l ++ [read acc :: Int]
numberList (x:xs) acc l | isDigit x = numberList xs (acc ++ [x]) l
                        | not (null acc) = numberList xs [] (l ++ [read acc :: Int])
                        | otherwise = numberList xs [] l

getNumber :: [Char] -> [Char] -> Int
getNumber [] [] = -1
getNumber [] acc = read acc :: Int
getNumber (x:xs) acc | isDigit x = getNumber xs (acc ++ [x])
                      | otherwise = getNumber xs acc

tryAnswer :: [Int] -> Int -> Int -> Int
tryAnswer [] a acc | a == acc = a
                    | otherwise = 0
tryAnswer (n:ns) a acc | plus == a || mul == a = a
                        | otherwise = 0
                        where
                          plus = tryAnswer ns a (acc + n)
                          mul = tryAnswer ns a (acc * n)