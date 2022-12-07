module Task1Lib (task1Func) where

import qualified UtilLib (readInt)
import Data.Char (ord, isLower)
import qualified Data.Char as UtilLib

task1Func :: [String] -> IO ()
task1Func fileLines = do
    putStrLn "Counts: "
    print $ count1s $ toIntArrs fileLines
    print $ count0s $ toIntArrs fileLines
    putStrLn "Most and least common bits: "
    print $ findMostAndLeastCommonBits fileLines
    putStrLn "Gamma and Epsilon: "
    print $ calcGammaEpsilon $ findMostAndLeastCommonBits fileLines
    putStrLn "Power consumption: "
    print $ calcPowerConsumption $ calcGammaEpsilon $ findMostAndLeastCommonBits fileLines

calcPowerConsumption :: (Int, Int) -> Int
calcPowerConsumption (x, y) = x * y

calcGammaEpsilon :: ([Bool], [Bool]) -> (Int, Int)
calcGammaEpsilon (bs1, bs2) = (binToDec bs1, binToDec bs2)

findMostAndLeastCommonBits :: [String] -> ([Bool], [Bool])
findMostAndLeastCommonBits strs = (mostCommonBits, leastCommonBits)
    where oneCounts = count1s $ toIntArrs strs
          zeroCounts = count0s $ toIntArrs strs
          mostCommonBits = zipWith (>=) oneCounts zeroCounts
          leastCommonBits = zipWith (<) oneCounts zeroCounts

toIntArrs :: [String] -> [[Int]]
toIntArrs = map $ map UtilLib.digitToInt

count1s :: [[Int]] -> [Int]
count1s intArrs = foldl count1sFold (initialCount intArrs) intArrs

count1sFold :: [Int] -> [Int] -> [Int]
count1sFold = zipWith (\x y -> if y == 1 then x + 1 else x)

count0s :: [[Int]] -> [Int]
count0s intArrs = foldl count0sFold (initialCount intArrs) intArrs

count0sFold :: [Int] -> [Int] -> [Int]
count0sFold = zipWith (\x y -> if y == 0 then x + 1 else x)

initialCount :: [[Int]] -> [Int]
initialCount intArrs = replicate (length (head intArrs)) 0

binToDec :: [Bool] -> Int
binToDec = foldl (\x y -> 2 * x + fromEnum y) 0
