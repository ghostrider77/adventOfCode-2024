import Data.List (find)
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Sequence ((|>), ViewL ((:< ), EmptyL))
import qualified Data.Map as M
import qualified Data.Sequence as Seq

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Tile = Empty | Wall deriving Eq

data Puzzle = Puzzle { maze :: Map Coord Tile, nrRows :: Int, nrCols :: Int, start :: Coord, target :: Coord }


parseInput :: [String] -> Puzzle
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrRows = length ls
        nrCols = length line
        cells = do
            (row, ix) <- zip ls [0..nrRows-1]
            (char, jy) <- zip row [0..nrCols-1]
            return (Coord ix jy, char)
        tiles = map (\(coord, char) -> (coord, if char == '#' then Wall else Empty)) cells
        start = fst $ fromJust $ find (\(_, char) -> char == 'S') cells
        target = fst $ fromJust $ find (\(_, char) -> char == 'E') cells
    in Puzzle {maze = M.fromList tiles, nrRows, nrCols, start, target}


calcDistances :: Puzzle -> Map Coord Int
calcDistances Puzzle {maze, nrRows, nrCols, start} =
    let isValid (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        go queue distances = case Seq.viewl queue of
            EmptyL -> distances
            (coord@(Coord x y) :< rest) ->
                let neighbors = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
                    d = distances ! coord
                    validNeighbors = filter (\c -> isValid c && maze ! c == Empty && M.notMember c distances) neighbors
                    distances' = foldl (\acc n -> M.insert n (d + 1) acc) distances validNeighbors
                    queue' = foldl (|>) rest validNeighbors
                in go queue' distances'
    in go (Seq.singleton start) (M.singleton start 0)


getNrShorterPaths :: Puzzle -> Int -> Int -> Int
getNrShorterPaths puzzle@Puzzle {start, target} maxCheatTime save =
    let distancesFromStart = calcDistances puzzle
        distancesFromTarget = calcDistances puzzle {start = target, target = start}
        targetDistance = distancesFromStart ! target
        reachableCoords = M.keys distancesFromStart
        pairs = [(c1, c2) | c1 <- reachableCoords, c2 <- reachableCoords]
        isPairBelongsToShorterPath (c1@(Coord x1 y1), c2@(Coord x2 y2)) =
            let d1 = distancesFromStart ! c1
                d2 = distancesFromTarget ! c2
                dist = abs (x1 - x2) + abs (y1 - y2)
            in dist <= maxCheatTime && targetDistance - (d1 + d2 + dist) >= save
    in length $ filter isPairBelongsToShorterPath pairs


main :: IO ()
main = do
    content <- lines <$> getContents
    let cheatTime = 20
    let puzzle = parseInput content
    print $ getNrShorterPaths puzzle cheatTime 100
