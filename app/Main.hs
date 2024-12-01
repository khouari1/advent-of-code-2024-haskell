module Main where

import Day01Pt1 (day01Pt1)
import Day01Pt2 (day01Pt2)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "01P1" : _ -> day01Pt1
        "01P2" : _ -> day01Pt2
        _ -> error "Invalid arg"