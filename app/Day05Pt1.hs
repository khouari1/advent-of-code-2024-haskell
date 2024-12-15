module Day05Pt1 where

import Data.List
import Data.Ord
import Data.Function (on)

day05Pt1 :: IO ()
day05Pt1 = do
  contents <- readFile "data/day5-input.txt"
  let allLines = lines contents
  let splitLines = initialLines allLines []
  let allTuples = mapUntilEmpty $ fst splitLines
  let kvList = convertKVList allTuples
  let ns = processLines kvList (snd splitLines)
  let result = sum ns
  print result

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

processLines :: [(Int, [Int])] -> [String] -> [Int]
processLines [] _ = []
processLines _ [] = []
processLines m (x:xs) = processLine m x : processLines m xs

processLine :: [(Int, [Int])] -> [Char] -> Int
processLine [] [] = 0
processLine m xs = processNumbers m ns
          where ns = commaSplit xs []

processNumbers :: [(Int, [Int])] -> [Int] -> Int
processNumbers [] _ = 0
processNumbers _ [] = 0
processNumbers m ns | inOrder m [] ns = ns !! (length ns `div` 2)
                    | otherwise = 0

inOrder :: [(Int, [Int])] -> [Int] -> [Int] -> Bool
inOrder [] _ _ = False
inOrder _ _ [] = True
inOrder m b (n:ns) | ordered m n b = inOrder m (n : b) ns
                  | otherwise = False

ordered :: [(Int, [Int])] -> Int -> [Int] -> Bool
ordered [] _ _ = False
ordered _ _ [] = True
ordered m n ns | anyPresent ns arr = False
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