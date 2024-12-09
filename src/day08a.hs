import Control.Monad (guard)
import Data.List (subsequences)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data City = City { antennas :: Map Char [Coord], nrRows :: Int, nrCols :: Int } deriving Show


parseInput :: [String] -> City
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrCols = length line
        nrRows = length ls
        coords = do
            (row, ix) <- zip ls [0..]
            (char, jy) <- zip row [0..]
            guard (char /= '.')
            return (char, [Coord ix jy])
    in City {antennas = M.fromListWith (++) coords, nrRows, nrCols}


calcAntiNodesOfAFrequency :: City -> [Coord] -> Set Coord
calcAntiNodesOfAFrequency (City _ nrRows nrCols) locations =
    let isValidLocation (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        calcAntiNodes (Coord x0 y0) (Coord x1 y1) =
            let dx = x1 - x0
                dy = y1 - y0
            in S.fromList $ filter isValidLocation [Coord (x1 + dx) (y1 + dy), Coord (x0 - dx) (y0 - dy)]
        addAntiNodes [c1, c2] = calcAntiNodes c1 c2
        addAntiNodes _ = S.empty
        locationPairs = filter ((==2) . length) $ subsequences locations
    in foldl (\acc pair -> S.union acc $ addAntiNodes pair) S.empty locationPairs


getNrAntiNodeLocations :: City -> Int
getNrAntiNodeLocations city@(City locations _ _) =
    let frequencyLocations = M.elems locations
        antiNodes = foldl (\acc cs -> S.union acc (calcAntiNodesOfAFrequency city cs)) S.empty frequencyLocations
    in S.size antiNodes


main :: IO ()
main = do
    content <- lines <$> getContents
    let city = parseInput content
    print $ getNrAntiNodeLocations city
