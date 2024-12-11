convertToIntegerList :: String -> [Int]
convertToIntegerList = map read . words


blinkOfAnEye :: [Int] -> Int -> Int
blinkOfAnEye stones n =
    let transformStone stone
            | stone == 0 = [1]
            | otherwise =
                let s = show stone
                    len = length s
                    k = len `div` 2
                in if even len then [read $ take k s, read $ drop k s] else [2024 * stone]
    in length $ iterate (concatMap transformStone) stones !! n


main :: IO ()
main = do
    content <- getLine
    let stones = convertToIntegerList content
    let nrRounds = 25
    print $ blinkOfAnEye stones nrRounds
