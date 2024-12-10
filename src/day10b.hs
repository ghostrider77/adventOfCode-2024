import Data.Char (digitToInt)
import Data.Map (Map, (!))
import qualified Data.Map as M

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


calcTrailHeadRating :: Puzzle -> Coord -> Int
calcTrailHeadRating puzzle@(Puzzle {topographicMap, nrRows, nrCols}) trailHead =
    let isValidCoord (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        getNextPositions (coord@(Coord x y) : _) =
            let value = topographicMap ! coord
                potentialNeighbors = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
            in filter (\c -> isValidCoord c && topographicMap ! c == value + 1) potentialNeighbors
        getNextPositions [] = []
        go n trails
            | n == 9 = length trails
            | otherwise =
                let trails' = concatMap (\tr -> let coords = getNextPositions tr in map (: tr) coords) trails
                in go (n + 1) trails'
    in go 0 [[trailHead]]


calcSumOfTrailHeadScores :: Puzzle -> Int
calcSumOfTrailHeadScores puzzle@(Puzzle {topographicMap}) =
    let trailHeads = M.keys $ M.filter (==0) topographicMap
    in sum $ map (calcTrailHeadRating puzzle) trailHeads


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ calcSumOfTrailHeadScores puzzle
