module Day04Pt1 where

day04Pt1 :: IO ()
day04Pt1 = do
  contents <- readFile "data/day4-input.txt"
  let allLines = lines contents
  let width = length $ head allLines
  let height = length allLines
  let result = checkLetters allLines (height, width) (0, 0)
  print result

checkLetters :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkLetters grid (dx,dy) (x,y) | x < dx && y < dy = checkLetter grid (dx,dy) (x,y) + checkLetters grid (dx,dy) (x + 1,y)
                                | x >= dx && y < dy = checkLetters grid (dx,dy) (0,y + 1) -- move to next row
                                | otherwise = 0

checkLetter :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkLetter [] _ _ = 0
checkLetter grid d x = checkToLeft grid d x + checkToRight grid d x + checkAbove grid d x + checkBelow grid d x + checkTopLeftD grid d x + checkTopRightD grid d x + checkBottomLeftD grid d x + checkBottomRightD grid d x

-- todo: generify the direction checks
checkToRight :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkToRight [] _ _ = 0
checkToRight grid (dx,_) (x,y) | x + 3 < dx && grid !! y !! x == 'X' && grid !! y !! (x + 1) == 'M' && grid !! y !! (x + 2) == 'A' && grid !! y !!(x + 3) == 'S' = 1
                                | otherwise = 0

checkToLeft :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkToLeft [] _ _ = 0
checkToLeft grid (_,_) (x,y) | x - 3 >= 0 && grid !! y !! x == 'X' && grid !! y !! (x - 1) == 'M' && grid !! y !! (x - 2) == 'A' && grid !! y !! (x - 3) == 'S' = 1
                                | otherwise = 0

checkAbove :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkAbove [] _ _ = 0
checkAbove grid (_,_) (x,y) | y - 3 >= 0 && grid !! y !! x == 'X' && grid !! (y - 1) !! x == 'M' && grid !! (y - 2) !! x == 'A' && grid !! (y - 3) !! x == 'S' = 1
                                | otherwise = 0

checkBelow :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkBelow [] _ _ = 0
checkBelow grid (_,dy) (x,y) | y + 3 < dy && grid !! y !! x == 'X' && grid !! (y + 1) !! x == 'M' && grid !! (y + 2) !! x == 'A' && grid !! (y + 3) !! x == 'S' = 1
                                | otherwise = 0

checkTopLeftD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkTopLeftD [] _ _ = 0
checkTopLeftD grid (_,_) (x,y) | y - 3 >= 0 && x - 3 >= 0 && grid !! y !! x == 'X' && grid !! (y - 1) !! (x - 1) == 'M' && grid !! (y - 2) !! (x - 2) == 'A' && grid !! (y - 3) !! (x - 3) == 'S' = 1
                                  | otherwise = 0

checkTopRightD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkTopRightD [] _ _ = 0
checkTopRightD grid (dx,_) (x,y) | y - 3 >= 0 && x + 3 < dx && grid !! y !! x == 'X' && grid !! (y - 1) !! (x + 1) == 'M' && grid !! (y - 2) !! (x + 2) == 'A' && grid !! (y - 3) !! (x + 3) == 'S' = 1
                                  | otherwise = 0

checkBottomLeftD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkBottomLeftD [] _ _ = 0
checkBottomLeftD grid (_,dy) (x,y) | y + 3 < dy && x - 3 >= 0 && grid !! y !! x == 'X' && grid !! (y + 1) !! (x - 1) == 'M' && grid !! (y + 2) !! (x - 2) == 'A' && grid !! (y + 3) !! (x - 3) == 'S' = 1
                                    | otherwise = 0

checkBottomRightD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkBottomRightD [] _ _ = 0
checkBottomRightD grid (dx,dy) (x,y) | y + 3 < dy && x + 3 < dx && grid !! y !! x == 'X' && grid !! (y + 1) !! (x + 1) == 'M' && grid !! (y + 2) !! (x + 2) == 'A' && grid !! (y + 3) !! (x + 3) == 'S' = 1
                                      | otherwise = 0