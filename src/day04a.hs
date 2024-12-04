import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List (isPrefixOf, tails)

data Cell = Cell Int Int deriving (Eq, Ord)
data Board = Board {board :: Map Cell Char, nrRows :: Int, nrCols :: Int}
newtype SecretWord = SecretWord String


parseInput :: [String] -> Board
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrCols = length line
        nrRows = length ls
        cells = do
            (row, ix) <- zip ls [0..]
            (letter, jy) <- zip row [0..]
            return (Cell ix jy, letter)
    in Board {board = M.fromList cells, nrRows, nrCols}


getDownwardDiagonals :: Board -> [String]
getDownwardDiagonals (Board board nrRows nrCols) =
    let isValidCell (Cell x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        startCoords = Cell 0 0 : map (`Cell` 0) [1..(nrRows-1)] ++ map (Cell 0) [1..(nrCols-1)]
        calcDiagonal (Cell x0 y0) =
            let coords = takeWhile isValidCell $ map (\k -> Cell (x0 + k) (y0 + k)) [0..]
            in map (board !) coords
    in map calcDiagonal startCoords


getUpwardDiagonals :: Board -> [String]
getUpwardDiagonals (Board board nrRows nrCols) =
    let isValidCell (Cell x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        startCoords = map (`Cell` 0) [0..(nrRows-2)] ++ map (Cell (nrRows - 1)) [0..(nrCols-1)]
        calcDiagonal (Cell x0 y0) =
            let coords = takeWhile isValidCell $ map (\k -> Cell (x0 - k) (y0 + k)) [0..]
            in map (board !) coords
    in map calcDiagonal startCoords


countSecretWordOccurrences :: Board -> SecretWord -> Int
countSecretWordOccurrences brd@(Board board nrRows nrCols) (SecretWord word) =
    let countOccurrences line = length $ filter (isPrefixOf word) $ tails line
        allRows = map (\ix -> map (\jy -> board ! Cell ix jy) [0..(nrCols-1)]) [0..(nrRows-1)]
        allColumns = map (\jy -> map (\ix -> board ! Cell ix jy) [0..(nrRows-1)]) [0..(nrCols-1)]
        downwardDiagonals = getDownwardDiagonals brd
        upwardDiagonals = getUpwardDiagonals brd
        allDirections = allRows ++ allColumns ++ downwardDiagonals ++ upwardDiagonals
    in foldl (\acc line -> acc + countOccurrences line + countOccurrences (reverse line)) 0 allDirections


main :: IO ()
main = do
    content <- lines <$> getContents
    let board = parseInput content
    let word = SecretWord "XMAS"
    print $ countSecretWordOccurrences board word
