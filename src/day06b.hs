{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as S

import Prelude hiding (Left, Right)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = Up | Right | Down | Left deriving (Eq, Ord, Show)
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


isValidCoord :: Puzzle -> Coord -> Bool
isValidCoord (Puzzle _ nrRows nrCols) (Coord x y) = 0 <= x && x < nrRows && 0 <= y && y < nrCols


getCoords :: Puzzle -> Coord -> Direction -> [Coord]
getCoords puzzle (Coord x y) direction =
    let ray = case direction of
            Up -> map (\k -> Coord (x - k) y) [0..]
            Right -> map (\k -> Coord x (y + k)) [0..]
            Down -> map (\k -> Coord (x + k) y) [0..]
            Left -> map (\k -> Coord x (y - k)) [0..]
    in takeWhile (isValidCoord puzzle) ray


followTheGuard :: Puzzle -> Coord -> Maybe (Set (Coord, Direction))
followTheGuard puzzle@(Puzzle board nrRows nrCols) startCoord =
    let go visitedCoords coord direction =
            let lineCoordinates = getCoords puzzle coord direction
            in case find (\c -> board ! c == Obstacle) lineCoordinates of
                Nothing -> Just $ S.union visitedCoords (S.fromList $ map (, direction) lineCoordinates)
                Just obstacleCoord ->
                    let pathCoords = takeWhile (/= obstacleCoord) lineCoordinates
                        coord' = last pathCoords
                        direction' = getNextDirection direction
                        inLoop = any (\c -> S.member (c, direction) visitedCoords) $ S.fromList pathCoords
                        visitedCoords' = S.union visitedCoords (S.fromList $ map (, direction) pathCoords)
                    in if inLoop then Nothing else go visitedCoords' coord' direction'
    in go S.empty startCoord Up


nrLoopingGuards :: Puzzle -> Coord -> Int
nrLoopingGuards puzzle@(Puzzle board nrRows nrCols) startCoord =
    let visitedCoords = fromJust $ followTheGuard puzzle startCoord
        potentialObstacleCoord = filter (/= startCoord) $ S.toList $ S.map fst visitedCoords
        alteredPuzzles = map (\c -> Puzzle (M.insert c Obstacle board) nrRows nrCols) potentialObstacleCoord
    in foldl (\acc p -> if isNothing $ followTheGuard p startCoord then acc + 1 else acc) 0 alteredPuzzles


main :: IO ()
main = do
    content <- lines <$> getContents
    let (puzzle, startPosition) = parseInput content
    print $ nrLoopingGuards puzzle startPosition
