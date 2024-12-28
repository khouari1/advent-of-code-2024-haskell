module Main where

import System.Environment (getArgs)
import Day01Pt1 (day01Pt1)
import Day01Pt2 (day01Pt2)
import Day02Pt1 (day02Pt1)
import Day02Pt2 (day02Pt2)
import Day03Pt1 (day03Pt1)
import Day03Pt2 (day03Pt2)
import Day04Pt1 (day04Pt1)
import Day04Pt2 (day04Pt2)
import Day05Pt1 (day05Pt1)
import Day05Pt2 (day05Pt2)
import Day06Pt1 (day06Pt1)
import Day06Pt2 (day06Pt2)
import Day07Pt1 (day07Pt1)
import Day07Pt2 (day07Pt2)
import Day08Pt1 (day08Pt1)
import Day08Pt2 (day08Pt2)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "01P1" : _ -> day01Pt1
        "01P2" : _ -> day01Pt2
        "02P1" : _ -> day02Pt1
        "02P2" : _ -> day02Pt2
        "03P1" : _ -> day03Pt1
        "03P2" : _ -> day03Pt2
        "04P1" : _ -> day04Pt1
        "04P2" : _ -> day04Pt2
        "05P1" : _ -> day05Pt1
        "05P2" : _ -> day05Pt2
        "06P1" : _ -> day06Pt1
        "06P2" : _ -> day06Pt2
        "07P1" : _ -> day07Pt1
        "07P2" : _ -> day07Pt2
        "08P1" : _ -> day08Pt1
        "08P2" : _ -> day08Pt2
        _ -> error "Invalid arg"