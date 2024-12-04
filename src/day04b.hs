import Data.Map (Map, (!))
import qualified Data.Map as M

data Cell = Cell Int Int deriving (Eq, Ord)
data Board = Board {board :: Map Cell Char, nrRows :: Int, nrCols :: Int}


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


isXMasWindow :: Board -> Cell -> Bool
isXMasWindow (Board board _ _) cell@(Cell x y) =
    let topLeft = board ! cell
        topRight = board ! Cell x (y + 2)
        middle = board ! Cell (x + 1) (y + 1)
        bottomLeft = board ! Cell (x + 2) y
        bottomRight = board ! Cell (x + 2) (y + 2)
        config1 = topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S' && middle == 'A'
        config2 = topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S' && middle == 'A'
        config3 = topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M' && middle == 'A'
        config4 = topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M' && middle == 'A'
    in config1 || config2 || config3 || config4


countXMasWindows :: Board -> Int
countXMasWindows brd@(Board board nrRows nrCols) =
    sum [1 | ix <- [0..(nrRows-3)], jy <- [0..(nrCols-3)], isXMasWindow brd (Cell ix jy)]


main :: IO ()
main = do
    content <- lines <$> getContents
    let board = parseInput content
    print $ countXMasWindows board
