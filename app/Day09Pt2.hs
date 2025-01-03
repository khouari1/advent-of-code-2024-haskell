module Day09Pt2 where

import Data.Char (digitToInt)
import Debug.Trace
import Data.Heap (Heap, fromList, insert, size, deleteMin)

type Coord = (Int,Int)
type BlockAndFree = (Int,Int)

day09Pt2 :: IO ()
day09Pt2 = do
  contents <- readFile "data/day9-input.txt"
  let line = head $ lines contents
  let pairs = getPairs line
  let initial = initialString pairs 0
  let heaps = reverse $ createInitialHeaps initial 9
  print heaps
  let t = fst $ process initial heaps (length initial - 1)
  let result = checksum t 0
  print result

initialString :: [BlockAndFree] -> Int -> [String]
initialString [] _ = []
initialString ((x,y):xs) i = replicate x (show i) ++ replicate y "." ++ initialString xs (i+1)

checksum :: [String] -> Int -> Int
checksum [] _ = 0
checksum (x:xs) i | x /= "." = i * xInt + checksum xs (i + 1)
                    | otherwise = checksum xs (i + 1)
                      where
                        xInt = read x :: Int

getPairs :: String -> [BlockAndFree]
getPairs [] = []
getPairs [x] = [(digitToInt x,0)]
getPairs (x1:x2:xs) = (digitToInt x1,digitToInt x2) : getPairs xs

process :: [String] -> [Heap Int] -> Int -> ([String],[Heap Int])
process cs hs 0 = (cs,hs)
process cs hs i | trace ("i = " ++ show i) i - currentLength < 0 = (cs,hs)
                | n == "." = process cs hs (i - currentLength)
                | otherwise = process newChar newHeap (i - currentLength)
  where
    n = cs !! i
    currentLength = getCurrentLengthForCharReverse (take (i + 1) cs) n 0 i
    heapsThatFit = drop currentLength hs
    h = findHeap heapsThatFit n (i - currentLength) currentLength Nothing
    (newChar,newHeap) = maybeProcessHeap cs h hs n i currentLength

findHeap :: [Heap Int] -> String -> Int -> Int -> Maybe (Int, Heap Int) -> Maybe (Int,Heap Int)
findHeap [] _ _ _ Nothing = Nothing
findHeap [] _ _ _ (Just (a,b)) = Just (a,b)
findHeap (h:hs) c currentIndex heapIndex currentH | c == "." = Nothing
                                                  | size h > 0 && minimum h > currentIndex = findHeap hs c currentIndex (heapIndex + 1) currentH -- ignore
                                                  | size h > 0 && lowerThanHeap h currentH = findHeap hs c currentIndex (heapIndex + 1) (Just (heapIndex, h)) -- potential
                                                  | otherwise = findHeap hs c currentIndex (heapIndex + 1) currentH -- ignore

lowerThanHeap :: Heap Int -> Maybe (Int,Heap Int) -> Bool
lowerThanHeap _ Nothing = True
lowerThanHeap h (Just (_,otherH)) = minimum h < minimum otherH

maybeProcessHeap :: [String] -> Maybe (Int,Heap Int) -> [Heap Int] -> String -> Int -> Int -> ([String],[Heap Int])
maybeProcessHeap cs Nothing hs _ _ _ = (cs,hs)
maybeProcessHeap cs h hs n i currentLength = processHeap cs h hs n i currentLength

processHeap :: [String] -> Maybe (Int,Heap Int) -> [Heap Int] -> String -> Int -> Int -> ([String],[Heap Int])
processHeap cs Nothing hs _ _ _ = (cs,hs)
processHeap cs (Just (heapIndex,h)) hs n ci numLength = (newCharList,newHeapList)
  where
    hMin = minimum h
    dotLength = getCurrentLengthForChar (drop hMin cs) "." 0
    remainingDotsLength = dotLength - numLength
    otherH = hs !! remainingDotsLength
    nh = deleteMin h
    newOtherH = insert (hMin + numLength) otherH
    newHeapList = updateHeaps (heapIndex,nh) (remainingDotsLength,newOtherH) hs
    newCharList = take hMin cs ++ replicate numLength n ++ drop (hMin + numLength) (take (ci - numLength + 1) cs) ++ replicate numLength "." ++ drop (ci + 1) cs

updateHeaps :: (Int,Heap Int) -> (Int,Heap Int) -> [Heap Int] -> [Heap Int]
updateHeaps _ _ [] = []
updateHeaps (h1Index,h1) (h2Index,h2) hs | h1Index < h2Index = take h1Index hs ++ [h1] ++ drop (h1Index + 1) (take h2Index hs) ++ [h2] ++ drop (h2Index + 1) hs
                                          | otherwise = updateHeaps (h2Index,h2) (h1Index,h1) hs

createInitialHeaps :: [String] -> Int -> [Heap Int]
createInitialHeaps [] _ = []
createInitialHeaps _ 0 = [fromList []]
createInitialHeaps cs n = createInitialHeapsForN cs n 0 : createInitialHeaps cs (n - 1)

createInitialHeapsForN :: [String] -> Int -> Int -> Heap Int
createInitialHeapsForN [] _ _ = fromList []
createInitialHeapsForN [_] _ _ = fromList []
createInitialHeapsForN _ 0 _ = fromList []
createInitialHeapsForN (c:cs) n i | c == "." && currentLength == n = insert i (createInitialHeapsForN nextCs n (i + currentLength))
                                    | otherwise = createInitialHeapsForN nextCs n (i + currentLength)
                                    where
                                      currentLength = getCurrentLengthForChar (c:cs) c 0
                                      nextCs = drop (currentLength - 1) cs

getCurrentLengthForChar :: [String] -> String -> Int -> Int
getCurrentLengthForChar [] _ _ = 0
getCurrentLengthForChar (x:xs) c n | x /= c = n
                                    | otherwise = getCurrentLengthForChar xs c (n + 1)

getCurrentLengthForCharReverse :: [String] -> String -> Int -> Int -> Int
getCurrentLengthForCharReverse [] _ _ _ = 0
getCurrentLengthForCharReverse cs c n 0 | head cs /= c = n
                                        | otherwise = n + 1
getCurrentLengthForCharReverse cs c n i | x /= c = n
                                        | otherwise = getCurrentLengthForCharReverse cs c (n + 1) (i - 1)
                                        where
                                          x = cs !! i