module Day11Pt1 where

day11Pt1 :: IO ()
day11Pt1 = do
  contents <- readFile "data/day11-input.txt"
  let numbers = map (\x -> read x :: Int) $ words $ head (lines contents)
  let result = length $ process numbers 0
  print result

process :: [Int] -> Int -> [Int]
process xs 25 = xs
process xs i = process (processLine xs) (i + 1)

processLine :: [Int] -> [Int]
processLine [] = []
processLine (x:xs) | x == 0 = 1 : processLine xs
                    | even xLength = newLeftNumber : newRightNumber : processLine xs
                    | otherwise = (x * 2024) : processLine xs
                    where
                      xString = show x
                      xLength = length xString
                      halfXLength = xLength `div` 2
                      newLeftNumber = read (take halfXLength xString) :: Int
                      newRightNumber = read (drop halfXLength xString) :: Int