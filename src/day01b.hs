{-# LANGUAGE TupleSections #-}

import qualified Data.IntMap as M


parseInput :: [String] -> [(Int, Int)]
parseInput ls =
    let parse line = case words line of
            [a, b] -> (read a, read b)
            _ -> error "Malformed input"
    in map parse ls


calcSimilarityScore :: [(Int, Int)] -> Int
calcSimilarityScore pairs =
    let (ids1, ids2) = unzip pairs
        idCounts = M.fromListWith (+) $ map (, 1) ids2
    in foldl (\acc id -> acc + id * M.findWithDefault 0 id idCounts) 0 ids1


main :: IO ()
main = do
    content <- lines <$> getContents
    let idPairs = parseInput content
    print $ calcSimilarityScore idPairs
