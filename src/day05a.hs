import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List.Split (splitOn)

type Rules = IntMap [Int]


parseInput :: [String] -> (Rules, [[Int]])
parseInput ls =
    let parseRule line = case splitOn "|" line of
            [a, b] -> (read a, [read b])
            _ -> error "Malformed input."
        parsePages line = map read $ splitOn "," line
        (rs, rest) = span (/= "") ls
    in (M.fromListWith (++) $ map parseRule rs, map parsePages $ tail rest)


isInCorrectOrder :: Rules -> [Int] -> Bool
isInCorrectOrder rules pages =
    let go preceedingPages [] = True
        go preceedingPages (a : rest) =
            let shouldBeAfter = M.findWithDefault [] a rules
                violated = any (`elem` preceedingPages) shouldBeAfter
            in not violated && go (a : preceedingPages) rest
    in go [] pages


calcMiddlePageSum :: Rules -> [[Int]] -> Int
calcMiddlePageSum rules ps =
    let middlePage pages = let n = length pages in pages !! (n `div` 2)
    in sum $ map middlePage $ filter (isInCorrectOrder rules) ps


main :: IO ()
main = do
    content <- lines <$> getContents
    let (rules, updates) = parseInput content
    print $ calcMiddlePageSum rules updates
