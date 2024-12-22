import Data.Bits (xor)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

type ConsecutiveChanges = (Int, Int, Int, Int)


modulus :: Int
modulus = 16777216


parseInput :: [String] -> [Int]
parseInput = map read


generateNext :: Int -> Int
generateNext secret =
    let mixAndPrune a b = (a `xor` b) `mod` modulus
        s1 = mixAndPrune secret (64 * secret)
        s2 = mixAndPrune s1 (s1 `div` 32)
        s3 = mixAndPrune s2 (s2 * 2048)
    in s3


calcPriceChangeRewards :: Int -> Int -> Map ConsecutiveChanges Int -> Map ConsecutiveChanges Int
calcPriceChangeRewards seed n mapping =
    let prices = map (`mod` 10) $ take (n + 1) $ iterate generateNext seed
        diffs = zipWith (-) (tail prices) prices
        go acc visited ((_, d1) : a@(_, d2) : b@(_, d3) : c@(p, d4) : rest) =
            let key = (d1, d2, d3, d4)
            in if S.member key visited then go acc visited (a : b : c : rest)
            else go (M.insertWith (+) key p acc) (S.insert key visited) (a : b : c : rest)
        go acc _ _ = acc
    in go mapping S.empty $ zip (tail prices) diffs


maximizeBananas :: [Int] -> Int -> Int
maximizeBananas seeds n = maximum $ M.elems $ foldl (\acc seed -> calcPriceChangeRewards seed n acc) M.empty seeds


main :: IO ()
main = do
    content <- lines <$> getContents
    let seeds = parseInput content
    let n = 2000
    print $ maximizeBananas seeds n
