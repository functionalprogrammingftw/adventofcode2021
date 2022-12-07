module MyLib (countFileSingleIncrements, countFileMultiIncrements) where

countFileSingleIncrements :: IO ()
countFileSingleIncrements = do
    contents <- readFile "src/data.txt"
    let list = map readInt . words $ contents
    let res = countSingleIncrements list
    print res

countFileMultiIncrements :: IO ()
countFileMultiIncrements = do
    contents <- readFile "src/data.txt"
    let list = map readInt . words $ contents
    let res = countMultiIncrements list
    print res

readInt :: String -> Int
readInt = read

countSingleIncrements :: [Int] -> Int
countSingleIncrements [] = 0
countSingleIncrements [x1] = 0
countSingleIncrements (x1:x2:xs)
  | x1 < x2 = 1 + countSingleIncrements (x2:xs)
  | otherwise = countSingleIncrements (x2:xs)

countMultiIncrements :: [Int] -> Int
countMultiIncrements [] = 0
countMultiIncrements [x1] = 0
countMultiIncrements [x1, x2] = 0
countMultiIncrements [x1, x2, x3] = 0
countMultiIncrements (x1:x2:x3:x4:xs)
  | x1 + x2 + x3 < x2 + x3 + x4 = 1 + countMultiIncrements (x2:x3:x4:xs)
  | otherwise = countMultiIncrements (x2:x3:x4:xs)
