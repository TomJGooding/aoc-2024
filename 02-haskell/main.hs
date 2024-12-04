import System.Environment
import Text.Printf

levelsFromReport :: String -> [Int]
levelsFromReport str = map read $ words str

isSafeLevels :: [Int] -> Bool
isSafeLevels (x : xs) = checkLevels xs x True True
    where
        checkLevels [] _ _ _ = True
        checkLevels (y : ys) prev inc dec
            | abs (y - prev) > 3 = False
            | inc && y > prev = checkLevels ys y True False
            | dec && y < prev = checkLevels ys y False True
            | otherwise = False

removeAtIndex :: Int -> [Int] -> [Int]
removeAtIndex i xs = take i xs ++ drop (i + 1) xs

isSafeDampened :: [Int] -> Bool
isSafeDampened xs
    | isSafeLevels xs = True
    | otherwise = any isSafeLevels [removeAtIndex i xs | i <- [0 .. length xs - 1]]

main = do
    args <- getArgs
    case args of
        (inputFile:_) -> do
            reports <- map levelsFromReport . lines <$> readFile inputFile
            putStrLn "--- Day 2: Red-Nosed Reports ---"
            printf "Answer for part 1: %d\n" $ length $ filter isSafeLevels reports
            printf "Answer for part 2: %d\n" $ length $ filter isSafeDampened reports
        _ -> do
            error "Input file not provided"
