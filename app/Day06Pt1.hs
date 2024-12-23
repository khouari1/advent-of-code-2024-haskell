module Day06Pt1 where

type Grid = [[Char]]
type Row = [Char]
data Direction = U | D | L | R

day06Pt1 :: IO ()
day06Pt1 = do
  contents <- readFile "data/day6-input.txt"
  let grid = lines contents
  let startingCoord = findStartingCoordLines grid 0
  let areasBeen = move grid startingCoord U
  let numberUniqueAreasBeen = length $ getDistinct areasBeen
  print numberUniqueAreasBeen

findStartingCoordLines :: Grid -> Int -> (Int,Int)
findStartingCoordLines [] _ = (0,0)
findStartingCoordLines (x:xs) yIndex | foundXIndex == -1 = findStartingCoordLines xs (yIndex + 1)
                                      | otherwise = (foundXIndex, yIndex)
                                      where foundXIndex = findStartingCoord x 0

findStartingCoord :: Row -> Int -> Int
findStartingCoord [] _ = -1
findStartingCoord (x:xs) xIndex | x == '^' = xIndex
                                  | otherwise = findStartingCoord xs (xIndex + 1)

move :: Grid -> (Int,Int) -> Direction -> [(Int,Int)]
move grid (currentX,currentY) U | isInGrid grid (currentX,currentY-1) && isBlocker grid (currentX,currentY-1) = move grid (currentX,currentY) R
                                | isInGrid grid (currentX,currentY-1) = (currentX,currentY) : move grid (currentX,currentY-1) U
                                | otherwise = [(currentX,currentY)]
move grid (currentX,currentY) D | isInGrid grid (currentX,currentY+1) && isBlocker grid (currentX,currentY+1) = move grid (currentX,currentY) L
                                | isInGrid grid (currentX,currentY+1) = (currentX,currentY) : move grid (currentX,currentY+1) D
                                | otherwise = [(currentX,currentY)]
move grid (currentX,currentY) L | isInGrid grid (currentX-1,currentY) && isBlocker grid (currentX-1,currentY) = move grid (currentX,currentY) U
                                | isInGrid grid (currentX-1,currentY) = (currentX,currentY) : move grid (currentX-1,currentY) L
                                | otherwise = [(currentX,currentY)]
move grid (currentX,currentY) R | isInGrid grid (currentX+1,currentY) && isBlocker grid (currentX+1,currentY) = move grid (currentX,currentY) D
                                | isInGrid grid (currentX+1,currentY) = (currentX,currentY) : move grid (currentX+1,currentY) R
                                | otherwise = [(currentX,currentY)]

isInGrid :: Grid -> (Int,Int) -> Bool
isInGrid grid (x,y) = y <= length grid - 1 && y >= 0 && x <= length (head grid) - 1 && x >= 0

isBlocker :: Grid -> (Int,Int) -> Bool
isBlocker grid (x,y) = (grid !! y !! x) == '#'

getDistinct :: [(Int,Int)] -> [(Int,Int)]
getDistinct [] = []
getDistinct (x:xs) | x `elem` xs = getDistinct xs
                    | otherwise = x : getDistinct xs