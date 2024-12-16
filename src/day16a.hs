import Data.List (find)
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = North | East | South | West deriving (Eq, Ord, Show)
data Distance = Dist Int | Infinity deriving Eq
data Position = Position { position :: Coord, direction :: Direction } deriving (Eq, Ord, Show)
data Tile = Empty | Wall deriving (Eq, Show)
data Puzzle = Puzzle { maze :: Map Coord Tile, startPosition :: Position, targetCoord :: Coord } deriving Show

data State = State { queue :: Set (Distance, Position)
                   , distances :: Map Position Distance
                   , finalizedPositions :: Set Position }


instance Show Distance where
    show :: Distance -> String
    show Infinity = "inf"
    show (Dist d) = show d


instance Ord Distance where
    (<=) :: Distance -> Distance -> Bool
    Infinity <= Infinity = True
    Dist d1 <= Dist d2 = d1 <= d2
    Infinity <= Dist _ = False
    Dist _ <= Infinity = True


costOfTurn :: Int
costOfTurn = 1000


readTile :: Char -> Tile
readTile '#' = Wall
readTile _ = Empty


parseInput :: [String] -> Puzzle
parseInput [] = error "No input received."
parseInput ls =
    let cells = do
            (row, ix) <- zip ls [0..]
            (char, jy) <- zip row [0..]
            return (Coord ix jy, char)
        tiles = map (\(coord, char) -> (coord, readTile char)) cells
        startCoord = fst $ fromJust $ find (\(_, char) -> char == 'S') cells
        startPosition = Position {position = startCoord, direction = East}
        targetCoord = fst $ fromJust $ find (\(_, char) -> char == 'E') cells
    in Puzzle {maze = M.fromList tiles, startPosition, targetCoord}


addDist :: Distance -> Distance -> Distance
addDist (Dist d1) (Dist d2) = Dist (d1 + d2)
addDist _ _ = Infinity


getNeighbors :: Map Coord Tile -> Position -> [(Position, Int)]
getNeighbors maze Position {position = coord@(Coord x y), direction} = case direction of
    North ->
        let turns =
                [(Position {position = coord, direction = East}, costOfTurn)
                ,(Position {position = coord, direction = West}, costOfTurn)]
            coord' = Coord (x - 1) y
        in if maze ! coord' == Empty then (Position {position = coord', direction}, 1) : turns else turns
    East ->
        let turns = [(Position {position = coord, direction = South}, costOfTurn)
                    ,(Position {position = coord, direction = North}, costOfTurn)]
            coord' = Coord x (y + 1)
        in if maze ! coord' == Empty then (Position {position = coord', direction}, 1) : turns else turns
    South ->
        let turns = [(Position {position = coord, direction = West}, costOfTurn)
                    ,(Position {position = coord, direction = East}, costOfTurn)]
            coord' = Coord (x + 1) y
        in if maze ! coord' == Empty then (Position {position = coord', direction}, 1) : turns else turns
    West ->
        let turns = [(Position {position = coord, direction = North}, costOfTurn)
                    ,(Position {position = coord, direction = South}, costOfTurn)]
            coord' = Coord x (y - 1)
        in if maze ! coord' == Empty then (Position {position = coord', direction}, 1) : turns else turns


updateDistances :: State -> [(Position, Int)] -> Distance -> State
updateDistances state neighbors dist =
    let go currentState [] = currentState
        go currentState@State {queue, distances} ((position, weight) : ns) =
            let distanceThroughNode = addDist dist (Dist weight)
                currentDistance = M.findWithDefault Infinity position distances
            in if currentDistance <= distanceThroughNode then go currentState ns
            else
                let queue' = S.insert (distanceThroughNode, position) queue
                    distances' =  M.insert position distanceThroughNode distances
                in go currentState {queue = queue', distances = distances'} ns
    in go state neighbors


calcShortestDistance :: Puzzle -> Distance
calcShortestDistance Puzzle {maze, startPosition, targetCoord} =
    let go state@State {queue, distances, finalizedPositions} =
            case S.lookupMin queue of
                Nothing ->
                    let targetPositions = filter (\(p, dist) -> position p == targetCoord) $ M.assocs distances
                    in if null targetPositions then Infinity else minimum $ map snd targetPositions
                Just (dist, position) ->
                    if S.member position finalizedPositions then go state {queue = S.deleteMin queue}
                    else
                        let neighbors = getNeighbors maze position
                            state' = updateDistances state neighbors dist
                        in go state' {finalizedPositions = S.insert position finalizedPositions}
    in go $ State (S.singleton (Dist 0, startPosition)) (M.singleton startPosition (Dist 0)) S.empty


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ calcShortestDistance puzzle
