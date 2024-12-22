import Data.Bits (xor)


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


sumOfNthSecretNumbers :: [Int] -> Int -> Int
sumOfNthSecretNumbers seeds n =
    let nthNumber s = iterate generateNext s !! n
    in sum $ map nthNumber seeds


main :: IO ()
main = do
    content <- lines <$> getContents
    let seeds = parseInput content
    let n = 2000
    print $ sumOfNthSecretNumbers seeds n
