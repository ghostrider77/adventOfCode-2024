import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Distance = Dist Int | Infinity deriving Eq
data Tile = Empty | Wall deriving (Eq, Show)

data Puzzle = Puzzle { maze :: Map Coord Tile, nrRows :: Int, nrCols :: Int } deriving Show
data State = State { queue :: Set (Distance, Coord), distances :: Map Coord Distance, finalizedCoords :: Set Coord }


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


addDist :: Distance -> Distance -> Distance
addDist (Dist d1) (Dist d2) = Dist (d1 + d2)
addDist _ _ = Infinity


parseInput :: [String] -> [Coord]
parseInput content =
    let parseByte line = case splitOn "," line of
            [x, y] -> Coord (read x) (read y)
            _ -> error "Malformed input."
    in map parseByte content


createCorruptedMap :: [Coord] -> Int -> Int -> Puzzle
createCorruptedMap bytes nrRows nrCols =
    let fallingBytes = S.fromList $ take 1024 bytes
        coords = [Coord x y | x <- [0..nrCols-1], y <- [0..nrRows-1]]
        tiles = map (\c -> if S.member c fallingBytes then (c, Wall) else (c, Empty)) coords
    in Puzzle {maze = M.fromList tiles, nrRows, nrCols}


getNeighbors :: Puzzle -> Coord -> [Coord]
getNeighbors Puzzle {maze, nrRows, nrCols} (Coord x0 y0) =
    let isValid (Coord x y) = 0 <= x && x < nrCols && 0 <= y && y < nrRows
        neighbors = [Coord x0 (y0 - 1), Coord (x0 + 1) y0, Coord x0 (y0 + 1), Coord (x0 - 1) y0]
    in filter (\c -> isValid c && maze ! c == Empty) neighbors


updateDistances :: State -> [Coord] -> Distance -> State
updateDistances state neighbors dist =
    let go currentState [] = currentState
        go currentState@State {queue, distances} (coord : ns) =
            let distanceThroughNode = addDist dist (Dist 1)
                currentDistance = M.findWithDefault Infinity coord distances
            in if currentDistance <= distanceThroughNode then go currentState ns
            else
                let queue' = S.insert (distanceThroughNode, coord) queue
                    distances' =  M.insert coord distanceThroughNode distances
                in go currentState {queue = queue', distances = distances'} ns
    in go state neighbors


calcShortestDistance :: Puzzle -> Coord -> Coord -> Distance
calcShortestDistance puzzle startCoord targetCoord =
    let go state@State {queue, distances, finalizedCoords} =
            case S.lookupMin queue of
                Nothing -> M.findWithDefault Infinity targetCoord distances
                Just (dist, coord) ->
                    if S.member coord finalizedCoords then go state {queue = S.deleteMin queue}
                    else
                        let neighbors = getNeighbors puzzle coord
                            state' = updateDistances state neighbors dist
                        in go state' {finalizedCoords = S.insert coord finalizedCoords}
    in go $ State (S.singleton (Dist 0, startCoord)) (M.singleton startCoord (Dist 0)) S.empty


main :: IO ()
main = do
    content <- lines <$> getContents
    let fallingBytes = parseInput content
    let nrRows = 71
    let nrCols = 71
    let startCoord = Coord 0 0
    let endCoord = Coord (nrRows - 1) (nrCols - 1)
    let puzzle = createCorruptedMap fallingBytes nrRows nrCols
    print $ calcShortestDistance puzzle startCoord endCoord
