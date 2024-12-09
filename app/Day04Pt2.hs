module Day04Pt2 where

day04Pt2 :: IO ()
day04Pt2 = do
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
checkLetter grid d x | checkTopLeftD grid d x + checkTopRightD grid d x + checkBottomLeftD grid d x + checkBottomRightD grid d x >= 2 = 1
                      | otherwise = 0

checkTopLeftD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkTopLeftD [] _ _ = 0
checkTopLeftD grid (dx,dy) (x,y) | y - 1 >= 0 && x - 1 >= 0 && y + 1 < dy && x + 1 < dx && grid !! y !! x == 'A' && grid !! (y - 1) !! (x - 1) == 'M' && grid !! (y + 1) !! (x + 1) == 'S' = 1
                                | otherwise = 0

checkTopRightD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkTopRightD [] _ _ = 0
checkTopRightD grid (dx,dy) (x,y) | y - 1 >= 0 && x + 1 < dx && y + 1 < dy && x - 1 >= 0 && grid !! y !! x == 'A' && grid !! (y - 1) !! (x + 1) == 'M' && grid !! (y + 1) !! (x - 1) == 'S' = 1
                                  | otherwise = 0

checkBottomLeftD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkBottomLeftD [] _ _ = 0
checkBottomLeftD grid (dx,dy) (x,y) | y + 1 < dy && x - 1 >= 0 && y - 1 >= 0 && x + 1 < dx && grid !! y !! x == 'A' && grid !! (y + 1) !! (x - 1) == 'M' && grid !! (y - 1) !! (x + 1) == 'S' = 1
                                    | otherwise = 0

checkBottomRightD :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
checkBottomRightD [] _ _ = 0
checkBottomRightD grid (dx,dy) (x,y) | y + 1 < dy && x + 1 < dx && y - 1 >= 0 && x - 1 >= 0 && grid !! y !! x == 'A' && grid !! (y + 1) !! (x + 1) == 'M' && grid !! (y - 1) !! (x - 1) == 'S' = 1
                                      | otherwise = 0