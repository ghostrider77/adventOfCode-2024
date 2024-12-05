import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (sortBy)
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


reOrderPages :: Rules -> [Int] -> [Int]
reOrderPages rules pages =
    let comparePages p1 p2 =
            let afterP1 = M.findWithDefault [] p1 rules
                afterP2 = M.findWithDefault [] p2 rules
            in if p1 `notElem` afterP2 then LT else if p2 `notElem` afterP1 then GT else EQ
    in sortBy comparePages pages


calcMiddlePageSum :: Rules -> [[Int]] -> Int
calcMiddlePageSum rules ps =
    let middlePage pages = let n = length pages in pages !! (n `div` 2)
    in sum $ map (middlePage . reOrderPages rules) $ filter (not . isInCorrectOrder rules) ps


main :: IO ()
main = do
    content <- lines <$> getContents
    let (rules, updates) = parseInput content
    print $ calcMiddlePageSum rules updates
