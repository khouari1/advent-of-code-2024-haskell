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
        _ -> error "Invalid arg"