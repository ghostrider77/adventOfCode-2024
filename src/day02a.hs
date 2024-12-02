convertToIntList :: String -> [Int]
convertToIntList = map read . words


isReportSafe :: [Int] -> Bool
isReportSafe levels =
    let neighbors (a : b : rest) = (a, b) : neighbors (b : rest)
        neighbors _ = []
        adjacentLevels = neighbors levels
        isSafelyIncreasing = all (\(l1, l2) -> let diff = l2 - l1 in 1 <= diff && diff <= 3) adjacentLevels
        isSafelyDecreasing = all (\(l1, l2) -> let diff = l1 - l2 in 1 <= diff && diff <= 3) adjacentLevels
    in isSafelyIncreasing || isSafelyDecreasing


calcNrSafeReports :: [[Int]] -> Int
calcNrSafeReports = foldl (\acc report -> if isReportSafe report then acc + 1 else acc) 0


main :: IO ()
main = do
    content <- lines <$> getContents
    let reports = map convertToIntList content
    print $ calcNrSafeReports reports
