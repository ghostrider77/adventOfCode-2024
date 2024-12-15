import Data.List (find)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Prelude hiding (Left, Right)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = Up | Right | Down | Left deriving Show
data Tile = Empty | Wall | Box deriving (Eq, Show)
data State = State {warehouse :: Map Coord Tile, robotPosition :: Coord } deriving Show


readDirection :: Char -> Direction
readDirection '^' = Up
readDirection '>' = Right
readDirection 'v' = Down
readDirection '<' = Left
readDirection c = error ("Unknown direction: " ++ [c])


readTile :: Char -> Tile
readTile '.' = Empty
readTile '@' = Empty
readTile '#' = Wall
readTile 'O' = Box
readTile c = error ("Unknown tile: " ++ [c])


parseInput :: [String] -> (State, [Direction])
parseInput content =
    let parseWareHouse [] = error "No input received."
        parseWareHouse ls@(line : _) =
            let cells = do
                    (row, ix) <- zip ls [0..]
                    (char, jy) <- zip row [0..]
                    return (Coord ix jy, char)
                robot = fst $ fromJust $ find (\(_, tile) -> tile == '@') cells
                tiles = M.fromList cells
            in State {warehouse = M.map readTile tiles, robotPosition = robot}
        (first, second) = span (/= "") content
    in (parseWareHouse first, map readDirection $ concat $ tail second)


moveAttempt :: State -> Direction -> State
moveAttempt state@State {warehouse, robotPosition = Coord x y} direction =
    let getRay Up = map (\k -> Coord (x - k) y) [1..]
        getRay Right = map (\k -> Coord x (y + k)) [1..]
        getRay Down = map (\k -> Coord (x + k) y) [1..]
        getRay Left = map (\k -> Coord x (y - k)) [1..]
        rayCoords = getRay direction
        rayTiles = map (warehouse !) rayCoords
        in case span (\(_, tile) -> tile == Box) $ zip rayCoords rayTiles of
            ([], (emptyPosition, Empty) : _) -> state {robotPosition = emptyPosition}
            (_, (_, Wall) : _) -> state
            ((boxCoord, _) : _, (emptyPosition, Empty) : _) ->
                let warehouse' = M.insert boxCoord Empty $ M.insert emptyPosition Box warehouse
                in State {warehouse = warehouse', robotPosition = boxCoord}
            _ -> error "Impossible box positions."


calcCoordinateSum :: State -> [Direction] -> Int
calcCoordinateSum initialState directions =
    let State {warehouse} = foldl moveAttempt initialState directions
        boxes = M.keys $ M.filter (== Box) warehouse
    in sum $ map (\(Coord x y) -> 100*x + y) boxes


main :: IO ()
main = do
    content <- lines <$> getContents
    let (initialState, directions) = parseInput content
    print $ calcCoordinateSum initialState directions
