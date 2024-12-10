import Data.Char (digitToInt)
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord)
data Puzzle = Puzzle {topographicMap :: Map Coord Int, nrRows :: Int, nrCols :: Int}


parseInput :: [String] -> Puzzle
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrCols = length line
        nrRows = length ls
        cells = do
            (row, ix) <- zip ls [0..]
            (letter, jy) <- zip row [0..]
            return (Coord ix jy, digitToInt letter)
    in Puzzle {topographicMap = M.fromList cells, nrRows, nrCols}


calcTrailHeadScore :: Puzzle -> Coord -> Int
calcTrailHeadScore puzzle@(Puzzle {topographicMap, nrRows, nrCols}) trailHead =
    let isValidCoord (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        getNextPositions coord@(Coord x y) =
            let value = topographicMap ! coord
                potentialNeighbors = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
            in S.fromList $ filter (\c -> isValidCoord c && topographicMap ! c == value + 1) potentialNeighbors
        go n ps
            | n == 9 = S.size ps
            | otherwise = let ps' = S.foldl (\acc c -> S.union acc (getNextPositions c)) S.empty ps in go (n + 1) ps'
    in go 0 (S.singleton trailHead)


calcSumOfTrailHeadScores :: Puzzle -> Int
calcSumOfTrailHeadScores puzzle@(Puzzle {topographicMap}) =
    let trailHeads = M.keys $ M.filter (==0) topographicMap
    in sum $ map (calcTrailHeadScore puzzle) trailHeads


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ calcSumOfTrailHeadScores puzzle
