module Task2Lib (task2Func) where

import qualified UtilLib (readInt)
import Data.Char (ord, isLower)
import qualified Data.Char as UtilLib
import Data.Bifunctor (Bifunctor(bimap))

task2Func :: [String] -> IO ()
task2Func fileLines = do
    putStrLn "Counts: "
    print $ count1s $ toIntArrs fileLines
    print $ count0s $ toIntArrs fileLines
    putStrLn "Most common bits: "
    print $ findMostCommonBits $ toIntArrs fileLines
    putStrLn "Filter on most common bit in pos 0: "
    print $ filterOnMostCommonBitInPos 0 $ toIntArrs fileLines
    putStrLn "Ratings binary: "
    print $ calcRatingsBinary fileLines
    putStrLn "Ratings decimal: "
    print $ calcRatings fileLines
    putStrLn "Life support rating: "
    print $ calcLifeSupportRating fileLines

calcLifeSupportRating fileLines = uncurry (*) ratings
    where ratings = calcRatings fileLines

calcRatings fileLines = bimap binToDec binToDec ratingsBinary
    where ratingsBinary = calcRatingsBinary fileLines

calcRatingsBinary fileLines = (oxygenGeneratorRating, co2ScrubberRating)
    where oxygenGeneratorRating = head $ filterOnMostCommonBitInPoses 0 $ toIntArrs fileLines
          co2ScrubberRating = head $ filterOnLeastCommonBitInPoses 0 $ toIntArrs fileLines

filterOnLeastCommonBitInPoses :: Int -> [[Int]] -> [[Int]]
filterOnLeastCommonBitInPoses pos intArrs =
    if length intArrs == 1 then intArrs
    else filterOnLeastCommonBitInPoses nextPos filteredIntArrs
    where nextPos = pos + 1
          filteredIntArrs = filterOnLeastCommonBitInPos pos intArrs

filterOnLeastCommonBitInPos :: Int -> [[Int]] -> [[Int]]
filterOnLeastCommonBitInPos pos intArrs = filter (\intArr -> intArr !! pos == fromEnum (leastCommonBits !! pos)) intArrs
    where leastCommonBits = findLeastCommonBits intArrs

findLeastCommonBits :: [[Int]] -> [Bool]
findLeastCommonBits intArrs = zipWith (<) oneCounts zeroCounts
    where oneCounts = count1s intArrs
          zeroCounts = count0s intArrs

filterOnMostCommonBitInPoses :: Int -> [[Int]] -> [[Int]]
filterOnMostCommonBitInPoses pos intArrs =
    if length intArrs == 1 then intArrs
    else filterOnMostCommonBitInPoses nextPos filteredIntArrs
    where nextPos = pos + 1
          filteredIntArrs = filterOnMostCommonBitInPos pos intArrs

filterOnMostCommonBitInPos :: Int -> [[Int]] -> [[Int]]
filterOnMostCommonBitInPos pos intArrs = filter (\intArr -> intArr !! pos == fromEnum (mostCommonBits !! pos)) intArrs
    where mostCommonBits = findMostCommonBits intArrs

findMostCommonBits :: [[Int]] -> [Bool]
findMostCommonBits intArrs = zipWith (>=) oneCounts zeroCounts
    where oneCounts = count1s intArrs
          zeroCounts = count0s intArrs

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

binToDec :: [Int] -> Int
binToDec = foldl (\x y -> 2 * x + y) 0
