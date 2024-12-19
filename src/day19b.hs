import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M


parseInput :: [String] -> ([String], [String])
parseInput content =
    let patterns = splitOn ", " $ head content
        towelDesigns = drop 2 content
    in (patterns, towelDesigns)


getNrDifferentArrangements :: String -> [String] -> Int
getNrDifferentArrangements towel patterns =
    let go [] cache = (1, cache)
        go s cache = case M.lookup s cache of
            Just count -> (count, cache)
            Nothing ->
                let suffixes = mapMaybe (`stripPrefix` s) patterns
                    (count, cache') =
                        foldl (\(acc, c) suffix -> let (acc', c') = go suffix c in (acc + acc', c')) (0, cache) suffixes
                in (count, M.insert s count cache')
    in fst $ go towel M.empty


getTotalNrOfDifferentArrangements :: [String] -> [String] -> Int
getTotalNrOfDifferentArrangements towels patterns = sum $ map (`getNrDifferentArrangements` patterns) towels


main :: IO ()
main = do
    content <- lines <$> getContents
    let (patterns, towelDesigns) = parseInput content
    print $ getTotalNrOfDifferentArrangements towelDesigns patterns
