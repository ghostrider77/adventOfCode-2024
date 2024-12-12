{-# LANGUAGE TupleSections #-}

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Ratio (Ratio, (%))
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
        getCorners (Coord x y) =
            [ (x % 1 - 1 % 2, y % 1 - 1 % 2)
            , (x % 1 - 1 % 2, y % 1 + 1 % 2)
            , (x % 1 + 1 % 2, y % 1 + 1 % 2)
            , (x % 1 + 1 % 2, y % 1 - 1 % 2) ]
        corners = M.fromListWith (++) $ concatMap (\coord -> map (, [coord]) $ getCorners coord) $ S.toList coords
        nrSides1 = foldl (\acc cs -> let l = length cs in if l == 1 || l == 3 then acc + 1 else acc) 0 $ M.elems corners
        isValid [Coord x0 y0, Coord x1 y1] = x0 /= x1 && y0 /= y1
        isValid _ = False
        nrSides2 = length $ filter isValid $ M.elems corners
    in area * (nrSides1 + 2*nrSides2)


calcTotalFencingPrice :: Puzzle -> Int
calcTotalFencingPrice puzzle =
    let regions = calcRegions puzzle
    in sum $ map calcPrice regions


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ calcTotalFencingPrice puzzle
