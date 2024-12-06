import Data.List (find)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Prelude hiding (Left, Right)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = Up | Right | Down | Left deriving (Eq, Show)
data Field = Empty | Obstacle deriving (Eq, Show)
data Puzzle = Puzzle {board :: Map Coord Field, nrRows :: Int, nrCols :: Int}


readField :: Char -> Field
readField '#' = Obstacle
readField _ = Empty


parseInput :: [String] -> (Puzzle, Coord)
parseInput [] = error "No input received."
parseInput ls@(line : _) =
    let nrCols = length line
        nrRows = length ls
        cells = do
            (row, ix) <- zip ls [0..]
            (char, jy) <- zip row [0..]
            return (Coord ix jy, char)
        guardPosition = fst $ fromJust $ find (\(_, char) -> char == '^') cells
        board = M.map readField $ M.fromList cells
    in (Puzzle {board, nrRows, nrCols}, guardPosition)


getNextDirection :: Direction -> Direction
getNextDirection Up = Right
getNextDirection Right = Down
getNextDirection Down = Left
getNextDirection Left = Up


followTheGuard :: Puzzle -> Coord -> Int
followTheGuard (Puzzle board nrRows nrCols) startCoord =
    let isValidCoord (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols
        getCoords (Coord x y) direction =
            let ray = case direction of
                    Up -> map (\k -> Coord (x - k) y) [1..]
                    Right -> map (\k -> Coord x (y + k)) [1..]
                    Down -> map (\k -> Coord (x + k) y) [1..]
                    Left -> map (\k -> Coord x (y - k)) [1..]
            in takeWhile isValidCoord ray
        go visitedCoords coord direction =
            let lineCoordinates = getCoords coord direction
            in case find (\c -> board ! c == Obstacle) lineCoordinates of
                Nothing ->
                    let allVisitedCoords = S.union visitedCoords (S.fromList lineCoordinates)
                    in S.size allVisitedCoords
                Just obstacleCoord ->
                    let pathCoords = takeWhile (/= obstacleCoord) lineCoordinates
                        coord' = last pathCoords
                        direction' = getNextDirection direction
                    in go (S.union visitedCoords (S.fromList pathCoords)) coord' direction'
    in go (S.singleton startCoord) startCoord Up


main :: IO ()
main = do
    content <- lines <$> getContents
    let (puzzle, startPosition) = parseInput content
    print $ followTheGuard puzzle startPosition
