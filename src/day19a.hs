import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)


parseInput :: [String] -> ([String], [String])
parseInput content =
    let patterns = splitOn ", " $ head content
        towelDesigns = drop 2 content
    in (patterns, towelDesigns)


isDesignPossible :: String -> [String] -> Bool
isDesignPossible [] _ = True
isDesignPossible s patterns =
    let possibleSuffixes = mapMaybe (`stripPrefix` s) patterns
    in any (`isDesignPossible` patterns) possibleSuffixes


getNrPossiblePatterns :: [String] -> [String] -> Int
getNrPossiblePatterns towels patterns =
    foldl (\acc towel -> if isDesignPossible towel patterns then acc + 1 else acc) 0 towels


main :: IO ()
main = do
    content <- lines <$> getContents
    let (patterns, towelDesigns) = parseInput content
    print $ getNrPossiblePatterns towelDesigns patterns
