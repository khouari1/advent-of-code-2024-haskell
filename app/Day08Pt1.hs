module Day08Pt1 where

import Data.Map ( fromListWith, elems )
import Data.List (nub)

type Coord = (Int,Int)

day08Pt1 :: IO ()
day08Pt1 = do
  contents <- readFile "data/day8-input.txt"
  let allLines = lines contents
  let height = length allLines
  let width = length $ head allLines
  let result = length . nub . concatMap (processNode (width,height)) . mergeCoords $ getAllCoords allLines 0
  print result

processNode :: (Int,Int) -> [Coord] -> [(Int,Int)]
processNode _ [] = []
processNode gridCoords (c:cs) = processCoord gridCoords c cs ++ processNode gridCoords cs

processCoord :: (Int,Int) -> Coord -> [Coord] -> [(Int,Int)]
processCoord _ _ [] = []
processCoord gridCoords c (o:os) = an1 ++ an2 ++ n ++ p
  where
    an1 = antinode c o gridCoords
    an2 = antinode o c gridCoords
    n = processCoord gridCoords c os
    p = processCoord gridCoords o os

antinode :: Coord -> Coord -> (Int,Int) -> [(Int,Int)]
antinode (x1,y1) (x2,y2) (gx,gy) | newX >= 0 && newX < gx && newY >= 0 && newY < gy = [(newX,newY)]
                                  | otherwise = []
  where
    xDiff = x2 - x1
    yDiff = y2 - y1
    newX = x2 + xDiff
    newY = y2 + yDiff

count :: Eq a => a -> [a] -> Int
count x =  length . Prelude.filter (==x)

mergeCoords :: [(Char,Coord)] -> [[Coord]]
mergeCoords xs = elems $ fromListWith (++) [(k, [x]) | (k, x) <- xs]

getAllCoords :: [String] -> Int -> [(Char,Coord)]
getAllCoords [] _ = []
getAllCoords (x:xs) yCoord = getCoordsForLine x 0 yCoord ++ getAllCoords xs (yCoord + 1)

getCoordsForLine :: String -> Int -> Int -> [(Char,Coord)]
getCoordsForLine [] _ _ = []
getCoordsForLine (x:xs) xCoord yCoord | x /= '.' = (x,(xCoord,yCoord)) : getCoordsForLine xs (xCoord + 1) yCoord
                                      | otherwise = getCoordsForLine xs (xCoord + 1) yCoord