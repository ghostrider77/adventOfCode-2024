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


isReportAlmostSafe :: [Int] -> Bool
isReportAlmostSafe levels =
    let shortenedLevels =
            let n = length levels
            in map (\ix -> let (l1, l2) = splitAt ix levels in l1 ++ tail l2) [0..(n-1)]
    in any isReportSafe shortenedLevels


calcNrAlmostSafeReports :: [[Int]] -> Int
calcNrAlmostSafeReports = foldl (\acc report -> if isReportAlmostSafe report then acc + 1 else acc) 0


main :: IO ()
main = do
    content <- lines <$> getContents
    let reports = map convertToIntList content
    print $ calcNrAlmostSafeReports reports
