import Data.List (find, intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S

import Prelude hiding (Left, Right)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = Up | Right | Down | Left deriving (Eq, Show)
data Tile = Empty | Wall | BoxLeft | BoxRight deriving Eq
data State = State {warehouse :: Map Coord Tile, robotPosition :: Coord, nrRows :: Int, nrCols :: Int }

instance Show Tile where
    show :: Tile -> String
    show Empty = "."
    show Wall = "#"
    show BoxLeft = "["
    show BoxRight = "]"


instance Show State where
    show :: State -> String
    show State {warehouse, robotPosition, nrRows, nrCols} =
        let toString coord = if coord == robotPosition then "@" else show $ warehouse ! coord
            rows = map (\x -> concatMap (toString . Coord x) [0..nrCols-1]) [0..nrRows-1]
        in intercalate "\n" rows


readDirection :: Char -> Direction
readDirection '^' = Up
readDirection '>' = Right
readDirection 'v' = Down
readDirection '<' = Left
readDirection c = error ("Unknown direction: " ++ [c])


readTiles :: Char -> (Tile, Tile)
readTiles '.' = (Empty, Empty)
readTiles '@' = (Empty, Empty)
readTiles '#' = (Wall, Wall)
readTiles 'O' = (BoxLeft, BoxRight)
readTiles c = error ("Unknown tile: " ++ [c])


parseInput :: [String] -> (State, [Direction])
parseInput content =
    let parseWareHouse [] = error "No input received."
        parseWareHouse ls@(line : _) =
            let nrRows = length ls
                nrCols = 2 * length line
                widen (Coord i j, char) = let (u, v) = readTiles char in [(Coord i (2*j), u), (Coord i (2*j + 1), v)]
                cells = do
                    (row, ix) <- zip ls [0..]
                    (char, jy) <- zip row [0..]
                    return (Coord ix jy, char)
                Coord xr yr = fst $ fromJust $ find (\(_, tile) -> tile == '@') cells
            in State {warehouse = M.fromList $ concatMap widen cells, robotPosition = Coord xr (2*yr), nrRows, nrCols}
        (first, second) = span (/= "") content
    in (parseWareHouse first, map readDirection $ concat $ tail second)


getNextCoord :: Direction -> Coord -> Coord
getNextCoord direction (Coord x y) = case direction of
    Up -> Coord (x - 1) y
    Right -> Coord x (y + 1)
    Down -> Coord (x + 1) y
    Left -> Coord x (y - 1)


getAffectedCoords :: State -> Direction -> Set Coord
getAffectedCoords state@State {warehouse, robotPosition = robotCoord@(Coord x y)} direction =
    let isBox coord = let tile = warehouse ! coord in tile == BoxLeft || tile == BoxRight
        addNeighbor c0@(Coord x0 y0) =
            let tile = warehouse ! c0
                c1 = if tile == BoxLeft then Coord x0 (y0 + 1) else Coord x0 (y0 - 1)
            in S.fromList [c0, c1]
        go boxCoords currentRow =
            if S.null currentRow then S.delete robotCoord boxCoords
            else
                let sign = if direction == Up then -1 else 1
                    row = S.filter isBox $ S.map (\(Coord x y) -> Coord (x + sign) y) currentRow
                    nextRow = S.foldl (\acc coord -> S.union acc (addNeighbor coord)) S.empty row
                in go (S.union boxCoords currentRow) nextRow
    in  if direction == Left || direction == Right then
            let sign = if direction == Right then 1 else -1
                coords = map (\k -> Coord x (y + sign*k)) [1..]
            in S.fromList $ takeWhile isBox coords
        else go S.empty (S.singleton robotCoord)


moveBoxes :: State -> Direction -> Set Coord -> Maybe (Map Coord Tile)
moveBoxes state@State {warehouse} direction affectedBoxCoords =
    let boxes = M.fromSet (warehouse !) affectedBoxCoords
        movedBoxes = M.mapKeys (getNextCoord direction) boxes
    in if any (\coord -> warehouse ! coord == Wall) $ M.keys movedBoxes then Nothing else Just movedBoxes


moveAttempt :: State -> Direction -> State
moveAttempt state@State {warehouse, robotPosition} direction =
    let affectedBoxCoords = getAffectedCoords state direction
        nextPosition = getNextCoord direction robotPosition
        nextTile = warehouse ! nextPosition
    in if S.null affectedBoxCoords then if nextTile == Empty then state {robotPosition = nextPosition} else state
    else case moveBoxes state direction affectedBoxCoords of
        Nothing -> state
        Just movedBoxes ->
            let warehouse' = M.insert robotPosition Empty warehouse
                warehouse'' = foldl (\acc c -> M.insert c Empty acc) warehouse' affectedBoxCoords
                warehouse''' = M.union movedBoxes warehouse''
            in state {warehouse = warehouse''', robotPosition = nextPosition}


calcCoordinateSum :: State -> [Direction] -> Int
calcCoordinateSum initialState directions =
    let State {warehouse} = foldl moveAttempt initialState directions
        boxes = M.keys $ M.filter (== BoxLeft) warehouse
    in sum $ map (\(Coord x y) -> 100*x + y) boxes


main :: IO ()
main = do
    content <- lines <$> getContents
    let (initialState, directions) = parseInput content
    print $ calcCoordinateSum initialState directions
