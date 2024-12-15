module Day05Pt2 where

import Data.List
import Data.Ord
import Data.Function (on)

day05Pt2 :: IO ()
day05Pt2 = do
  contents <- readFile "data/day5-input.txt"
  let allLines = lines contents
  let splitLines = initialLines allLines []
  let allTuples = mapUntilEmpty $ fst splitLines
  let kvList = convertKVList allTuples
  let incorrectLines = removeEmptyLists $ processLines kvList $ snd splitLines
  let sortedLines = sortLines kvList incorrectLines
  let middleNumbers = map (\x -> x !! (length x `div` 2)) sortedLines
  let result = sum middleNumbers
  print result

sortLines :: [(Int, [Int])] -> [[Int]] -> [[Int]]
sortLines _ [] = []
sortLines m (x:xs) = sortAllNumbers m [] x : sortLines m xs

initialLines :: [String] -> [String] -> ([String], [String])
initialLines [] _ = ([],[])
initialLines (x:xs) acc | null x = (acc, xs)
                        | otherwise = initialLines xs (acc ++ [x])

mapUntilEmpty :: [String] -> [(Int, Int)]
mapUntilEmpty [] = []
mapUntilEmpty (x:xs) | null x = []
                      | otherwise = toIntPair x [] : mapUntilEmpty xs

toIntPair :: [Char] -> [Char] -> (Int, Int)
toIntPair [] _ = (0,0)
toIntPair (x:xs) acc | x == '|' = (read acc :: Int, read xs :: Int)
                      | otherwise = toIntPair xs (acc ++ [x])

processLines :: [(Int, [Int])] -> [String] -> [[Int]]
processLines [] _ = []
processLines _ [] = []
processLines m (x:xs) = processLine m x : processLines m xs

processLine :: [(Int, [Int])] -> [Char] -> [Int]
processLine [] [] = []
processLine m xs = processNumbers m ns
          where ns = commaSplit xs []

removeEmptyLists :: [[a]] -> [[a]]
removeEmptyLists [] = []
removeEmptyLists (x:xs) | null x = removeEmptyLists xs
                        | otherwise = x : removeEmptyLists xs

processNumbers :: [(Int, [Int])] -> [Int] -> [Int]
processNumbers [] _ = []
processNumbers _ [] = []
processNumbers m ns | allNumbersInOrder m [] ns = []
                    | otherwise = sortAllNumbers m [] ns

allNumbersInOrder :: [(Int, [Int])] -> [Int] -> [Int] -> Bool
allNumbersInOrder [] _ _ = False
allNumbersInOrder _ _ [] = True
allNumbersInOrder m b (n:ns) | nInOrder m n b = allNumbersInOrder m (n : b) ns
                              | otherwise = False

-- map -> accumulation -> original numbers -> sorted numbers
sortAllNumbers :: [(Int, [Int])] -> [Int] -> [Int] -> [Int]
sortAllNumbers [] _ _ = []
sortAllNumbers _ acc [] = acc
sortAllNumbers m acc (n:ns) | nInOrder m n currentList = sortAllNumbers m (acc ++ [n]) ns
                            | otherwise = sortAllNumbers m (sortN m n acc []) ns
                            where currentList = acc ++ (n:ns)

-- map -> number (n) to sort -> original numbers -> accumulation -> numbers with n sorted
sortN :: [(Int, [Int])] -> Int -> [Int] -> [Int] -> [Int]
sortN [] _ _ _ = []
sortN _ n [] acc = acc ++ [n]
sortN m n (x:xs) acc | x `elem` arr = acc ++ n : x : xs
                      | otherwise = sortN m n xs (acc ++ [x])
                    where arr = findN n m

-- map -> number (n) to check -> list of numbers -> is ordered
nInOrder :: [(Int, [Int])] -> Int -> [Int] -> Bool
nInOrder [] _ _ = False
nInOrder _ _ [] = True
nInOrder m n ns | anyPresent ns arr = False
                | otherwise = True
                where arr = findN n m

anyPresent :: [Int] -> [Int] -> Bool
anyPresent [] _ = False
anyPresent (n:ns) arr | n `elem` arr = True
                      | otherwise = anyPresent ns arr

commaSplit :: [Char] -> [Char] -> [Int]
commaSplit [] acc = [read acc :: Int]
commaSplit (x:xs) acc | x == ',' = (read acc :: Int) : commaSplit xs []
                      | otherwise = commaSplit xs (acc ++ [x])

convertKVList :: Ord a => [(a,b)] -> [(a,[b])]
convertKVList = map (\x -> (fst $ head x,  map snd x)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

findN :: Int -> [(Int, [Int])] -> [Int]
findN _ [] = []
findN n (x:xs) | n == fst x = snd x
                | otherwise = findN n xs