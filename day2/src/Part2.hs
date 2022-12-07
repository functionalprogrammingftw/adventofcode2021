module Part2 (calculatePositionFromFile) where
import Data.List

calculatePositionFromFile :: IO ()
calculatePositionFromFile = do
    contents <- readFile "src/data.txt"
    let list = lines contents
    let (hpos, depth, aim) = calculatePosition list
    putStrLn "Part2 hpos:"
    print hpos
    putStrLn "Part2 depth:"
    print depth
    putStrLn "Part2 aim:"
    print aim
    putStrLn "Part2 hpos * depth:"
    print $ hpos * depth

calculatePosition :: [String] -> (Int, Int, Int)
calculatePosition = foldl calculateSinglePosition (0, 0, 0)

calculateSinglePosition :: (Int, Int, Int) -> String -> (Int, Int, Int)
calculateSinglePosition (hpos, depth, aim) command | Just amount <- stripPrefix "forward " command =
    (hpos + read amount, depth + aim * read amount, aim)
calculateSinglePosition (hpos, depth, aim) command | Just amount <- stripPrefix "down " command =
    (hpos, depth, aim + read amount)
calculateSinglePosition (hpos, depth, aim) command | Just amount <- stripPrefix "up " command =
    (hpos, depth, aim - read amount)
calculateSinglePosition (hpos, depth, aim) command =
    (hpos, depth, aim)
