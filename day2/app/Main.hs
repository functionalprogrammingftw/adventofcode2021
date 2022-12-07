module Main where

import qualified Part1 (calculatePositionFromFile)
import qualified Part2 (calculatePositionFromFile)

main :: IO ()
main = do
  putStrLn ""
  Part1.calculatePositionFromFile
  putStrLn ""
  Part2.calculatePositionFromFile
