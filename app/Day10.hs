module Day10 where
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Data.List (nub)

type Map = [[Int]]
type Coord = (Int,Int)
type Row = [Int]

day10 :: IO ()
day10 = do
  contents <- readFile "data/day10-input.txt"
  let inputMap = map (map digitToInt) $ lines contents
  let trailHeadCoords = findTrailHeads inputMap 0
  let foundRoutes = map (findRoutes inputMap []) trailHeadCoords
  let part1Result = sum $ map (length . nub) foundRoutes
  let part2Result = sum $ map length foundRoutes
  print part1Result
  print part2Result

findTrailHeads :: Map -> Int -> [Coord]
findTrailHeads [] _ = []
findTrailHeads (m:ms) currentY = findTrailHeadsInRow m (0,currentY) ++ findTrailHeads ms (currentY + 1)

findTrailHeadsInRow :: Row -> Coord -> [Coord]
findTrailHeadsInRow [] _ = []
findTrailHeadsInRow (x:xs) (currentX,currentY) | x == 0 = (currentX,currentY) : findTrailHeadsInRow xs (currentX + 1, currentY)
                                                | otherwise = findTrailHeadsInRow xs (currentX + 1, currentY)

findRoutes :: Map -> [Coord] -> Coord -> [Coord]
findRoutes [] _ _ = []
findRoutes m beenCoords (x,y) | n == 9 = [(x,y)] -- found valid route
                              | null nextTries = [] -- finished route
                              | otherwise = concatMap (findRoutes m newBeen) nextTries
  where
    n = m !! y !! x
    newBeen = (x,y) : beenCoords
    nextTries = getNextTries m beenCoords (x,y)

getNextTries :: Map -> [Coord] -> Coord -> [Coord]
getNextTries [] _ _ = []
getNextTries m beenCoords (x,y) = catMaybes [tryLeft, tryRight, tryUp, tryDown]
  where
    canGoLeft = x - 1 >= 0 && notElem (x-1,y) beenCoords && m !! y !! (x-1) == currentPlusOne
    canGoRight = x + 1 < length (head m) && notElem (x+1,y) beenCoords && m !! y !! (x+1) == currentPlusOne
    canGoUp = y - 1 >= 0 && notElem (x,y-1) beenCoords && m !! (y - 1) !! x == currentPlusOne
    canGoDown = y + 1 < length m && notElem (x,y+1) beenCoords && m !! (y + 1) !! x == currentPlusOne
    tryLeft = if canGoLeft then Just (x-1,y) else Nothing
    tryRight = if canGoRight then Just (x+1,y) else Nothing
    tryUp = if canGoUp then Just (x,y-1) else Nothing
    tryDown = if canGoDown then Just (x,y+1) else Nothing
    currentPlusOne = (m !! y !! x) + 1