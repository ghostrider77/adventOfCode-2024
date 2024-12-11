import qualified Data.MemoTrie as M


convertToIntList :: String -> [Int]
convertToIntList = map read . words


transformStone :: Int -> [Int]
transformStone stone
    | stone == 0 = [1]
    | otherwise =
        let s = show stone
            len = length s
            k = len `div` 2
        in if even len then [read $ take k s, read $ drop k s] else [2024 * stone]


countStones :: [Int] -> Int -> Int
countStones stones n =
    let stoneTransformations =
            let go 0 = const 1
                go k = M.memo $ sum . map (stoneTransformations (k - 1)) . transformStone
            in go
    in sum $ map (stoneTransformations n) stones


main :: IO ()
main = do
    content <- getLine
    let stones = convertToIntList content
    let nrRounds = 75
    print $ countStones stones nrRounds
