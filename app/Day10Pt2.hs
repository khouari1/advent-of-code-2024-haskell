module Day10Pt2 where
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

type Map = [[Int]]
type Coord = (Int,Int)
type Row = [Int]

day10Pt2 :: IO ()
day10Pt2 = do
  contents <- readFile "data/day10-input.txt"
  let inputMap = map (map digitToInt) $ lines contents
  let trailHeadCoords = findTrailHeads inputMap 0
  let result = sum $ map (length . findRoutes inputMap []) trailHeadCoords
  print result

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
    tryLeft = maybeTry canGoLeft (x-1,y)
    tryRight = maybeTry canGoRight (x+1,y)
    tryUp = maybeTry canGoUp (x,y-1)
    tryDown = maybeTry canGoDown (x,y+1)
    currentPlusOne = (m !! y !! x) + 1

maybeTry :: Bool -> Coord -> Maybe Coord
maybeTry f c | f = Just c
              | otherwise = Nothing