{-# LANGUAGE TupleSections #-}

import Data.IntMap (IntMap)
import qualified Data.IntMap as M


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


oneBlink :: IntMap Int -> IntMap Int
oneBlink stoneCounts =
    let updatedCounts = do
            (stone, count) <- M.assocs stoneCounts
            stone' <- transformStone stone
            return (stone', count)
    in M.fromListWith (+) updatedCounts


countStones :: [Int] -> Int -> Int
countStones stones n =
    let initialCounts = M.fromList $ map (, 1) stones
    in sum $ M.elems $ iterate oneBlink initialCounts !! n


main :: IO ()
main = do
    content <- getLine
    let stones = convertToIntList content
    let nrRounds = 75
    print $ countStones stones nrRounds
