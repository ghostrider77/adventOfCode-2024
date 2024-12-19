import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.MemoTrie as M


parseInput :: [String] -> ([String], [String])
parseInput content =
    let patterns = splitOn ", " $ head content
        towelDesigns = drop 2 content
    in (patterns, towelDesigns)


getNrDifferentArrangements :: String -> [String] -> Int
getNrDifferentArrangements towel patterns =
    let arrangements = M.memo (\s -> if null s then 1 else sum (map arrangements (mapMaybe (`stripPrefix` s) patterns)))
    in arrangements towel


getTotalNrOfDifferentArrangements :: [String] -> [String] -> Int
getTotalNrOfDifferentArrangements towels patterns = sum $ map (`getNrDifferentArrangements` patterns) towels


main :: IO ()
main = do
    content <- lines <$> getContents
    let (patterns, towelDesigns) = parseInput content
    print $ getTotalNrOfDifferentArrangements towelDesigns patterns
