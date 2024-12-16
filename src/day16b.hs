import Data.Heap (MinPrioHeap)
import Data.List (find)
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Direction = North | East | South | West deriving (Eq, Ord, Show)
data Distance = Dist Int | Infinity deriving Eq
data Position = Position { position :: Coord, direction :: Direction } deriving (Eq, Ord, Show)
data Tile = Empty | Wall deriving (Eq, Show)
data Puzzle = Puzzle { maze :: Map Coord Tile, startPosition :: Position, targetCoord :: Coord } deriving Show

data State = State { queue :: MinPrioHeap Distance Position
                   , distances :: Map Position Distance
                   , finalizedPositions :: Set Position
                   , backtrack :: Map Position (Set Position)
                   }


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


updateDistances :: State -> Position -> Distance -> [(Position, Int)] -> State
updateDistances state pos dist neighbors =
    let alterFunc p Nothing = Just $ S.singleton p
        alterFunc p (Just ps) = Just $ S.insert p ps
        go currentState [] = currentState
        go currentState@State {queue, distances, backtrack} ((position, weight) : ns) =
            let distanceThroughNode = addDist dist (Dist weight)
                currentDistance = M.findWithDefault Infinity position distances
            in if currentDistance < distanceThroughNode then go currentState ns
            else if currentDistance == distanceThroughNode then
                let backtrack' = M.alter (alterFunc pos) position backtrack
                in go currentState {backtrack = backtrack'} ns
            else
                let queue' = H.insert (distanceThroughNode, position) queue
                    distances' = M.insert position distanceThroughNode distances
                    backtrack' = M.alter (alterFunc pos) position backtrack
                in go currentState {queue = queue', distances = distances', backtrack = backtrack'} ns
    in go state neighbors


calcShortestDistance :: Puzzle -> ([Position], Map Position (Set Position))
calcShortestDistance Puzzle {maze, startPosition, targetCoord} =
    let go state@State {queue, distances, finalizedPositions, backtrack} =
            case H.view queue of
                Nothing ->
                    let targetPositions = filter (\(p, dist) -> position p == targetCoord) $ M.assocs distances
                        minDist = minimum $ map snd targetPositions
                        targets = map fst $ filter(\(_, dist) -> dist == minDist) targetPositions
                    in (targets, backtrack)
                Just ((dist, position), rest) ->
                    if S.member position finalizedPositions then go state {queue = rest}
                    else
                        let neighbors = getNeighbors maze position
                            state' = updateDistances state position dist neighbors
                        in go state' { finalizedPositions = S.insert position finalizedPositions }
    in go $ State (H.singleton (Dist 0, startPosition)) (M.singleton startPosition (Dist 0)) S.empty M.empty


collectCoordsInShortestPaths :: Puzzle -> Int
collectCoordsInShortestPaths puzzle@Puzzle {startPosition} =
    let (targets, backtrack) = calcShortestDistance puzzle
        go acc currentPositions
            | S.null currentPositions = S.size $ S.map (\Position {position} -> position) acc
            | otherwise =
                let getItems position = M.findWithDefault S.empty position backtrack
                    nextPositions = S.foldl (\a p -> S.union a (getItems p)) S.empty currentPositions
                in go (S.union acc currentPositions) nextPositions
    in go S.empty (S.fromList targets)


main :: IO ()
main = do
    content <- lines <$> getContents
    let puzzle = parseInput content
    print $ collectCoordsInShortestPaths puzzle
