module Main where

import System.Environment (getArgs)
import Day01Pt1 (day01Pt1)
import Day01Pt2 (day01Pt2)
import Day02Pt1 (day02Pt1)
import Day02Pt2 (day02Pt2)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "01P1" : _ -> day01Pt1
        "01P2" : _ -> day01Pt2
        "02P1" : _ -> day02Pt1
        "02P2" : _ -> day02Pt2
        _ -> error "Invalid arg"