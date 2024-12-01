import Data.List (sort)


parseInput :: [String] -> [(Int, Int)]
parseInput ls =
    let parse line = case words line of
            [a, b] -> (read a, read b)
            _ -> error "Malformed input"
    in map parse ls


calcTotalIdDistance :: [(Int, Int)] -> Int
calcTotalIdDistance pairs =
    let (ids1, ids2) = unzip pairs
        sid1 = sort ids1
        sid2 = sort ids2
    in foldl (\acc (id1, id2) -> acc + abs (id1 - id2)) 0 $ zip sid1 sid2


main :: IO ()
main = do
    content <- lines <$> getContents
    let idPairs = parseInput content
    print $ calcTotalIdDistance idPairs
