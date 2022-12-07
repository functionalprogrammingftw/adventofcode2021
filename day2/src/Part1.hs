module Part1 (calculatePositionFromFile) where
import Data.List

calculatePositionFromFile :: IO ()
calculatePositionFromFile = do
    contents <- readFile "src/data.txt"
    let list = lines contents
    let (hpos, depth) = calculatePosition list
    putStrLn "Part1 hpos:"
    print hpos
    putStrLn "Part1 depth:"
    print depth
    putStrLn "Part1 hpos * depth:"
    print $ hpos * depth

calculatePosition :: [String] -> (Int, Int)
calculatePosition [] = (0, 0)
calculatePosition (x:xs) = (prevHpos + singleHpos, prevDepth + singleDepth)
    where (prevHpos, prevDepth) = calculatePosition xs
          (singleHpos, singleDepth) = calculateSinglePosition x


calculateSinglePosition :: String -> (Int, Int)
calculateSinglePosition string | Just restOfString <- stripPrefix "forward " string =
    (read restOfString, 0)
calculateSinglePosition string | Just restOfString <- stripPrefix "down " string =
    (0, read restOfString)
calculateSinglePosition string | Just restOfString <- stripPrefix "up " string =
    (0, -1 * read restOfString)
calculateSinglePosition string =
    (0, 0)
