import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Sequence ((|>), ViewL ((:< ), EmptyL))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Puzzle = Puzzle {garden :: Map Coord Char, nrRows :: Int, nrCols :: Int} deriving Show
newtype Region = Region (Set Coord) deriving Show


parseInput :: [String] -> Puzzle
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrCols = length line
        nrRows = length ls
        cells = do
            (row, ix) <- zip ls [0..]
            (letter, jy) <- zip row [0..]
            return (Coord ix jy, letter)
    in Puzzle {garden = M.fromList cells, nrRows, nrCols}


calcRegions :: Puzzle -> [Region]
calcRegions (Puzzle {garden, nrRows, nrCols}) =
    let isValidCoord (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        discover queue region char = case Seq.viewl queue of
            EmptyL -> region
            ((Coord x y) :< rest) ->
                let ns = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
                    validNeighbors = filter (\c -> isValidCoord c && garden ! c == char && S.notMember c region) ns
                    region' = foldl (flip S.insert) region validNeighbors
                    queue' = foldl (|>) rest validNeighbors
                in discover queue' region' char
        detectRegions acc _ [] = acc
        detectRegions acc visited (coord : cs)
            | S.member coord visited = detectRegions acc visited cs
            | otherwise =
                let region = discover (Seq.singleton coord) (S.singleton coord) (garden ! coord)
                    visited' = S.union visited region
                in detectRegions (Region region : acc) visited' cs
    in detectRegions [] S.empty (M.keys garden)


calcPrice :: Region -> Int
calcPrice (Region coords) =
    let area = S.size coords
        nrNeighborsInRegion (Coord x y) =
            let neighbors = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
            in foldl (\acc n -> if S.member n coords then acc + 1 else acc) 0 neighbors
        perimeter = 4 * area - S.foldl (\acc coord -> acc + nrNeighborsInRegion coord) 0 coords
    in area * perimeter


calcTotalFencingPrice :: Puzzle -> Int
calcTotalFencingPrice puzzle =
    let regions = calcRegions puzzle
    in sum $ map calcPrice regions


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ calcTotalFencingPrice puzzle
