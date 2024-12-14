import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Quadrant = TL | TR | BR | BL deriving (Eq, Ord)
data Robot = Robot { position :: Coord, vX :: Int, vY :: Int } deriving Show
data State = State { robots :: [Robot], nrRows :: Int, nrCols :: Int }


instance Show State where
    show :: State -> String
    show State {robots, nrRows, nrCols} =
        let coords = S.fromList $ map position robots
            char coord = if S.member coord coords then '*' else '.'
            rows = map (\y -> map (\x -> char $ Coord x y) [0..nrCols-1]) [0..nrRows-1]
        in intercalate "\n" rows


parseInput :: [String] -> [Robot]
parseInput ls =
    let splitCoords s = case splitOn "," s of
            [x, y] -> (read x, read y)
            _ -> error "Malformed input."
        parseLine line = case words line of
            [p, v] ->
                let (px, py) = splitCoords $ drop 2 p
                    (vx, vy) = splitCoords $ drop 2 v
                in Robot { position = Coord px py, vX = vx, vY = vy }
            _ -> error ("Malformed input line: " ++ line)
    in map parseLine ls


moveAllRobots :: State -> State
moveAllRobots state@State {robots, nrRows, nrCols} =
    let move robot@Robot {position = Coord x y, vX, vY} =
            let x' = (x + vX) `mod` nrCols
                y' = (y + vY) `mod` nrRows
            in robot {position = Coord x' y'}
    in state {robots = map move robots}


detectXMasTreeConfiguration :: State -> (Int, State)
detectXMasTreeConfiguration initialState@State {nrRows, nrCols} =
    let maxIter = nrRows * nrCols
        x1 = nrCols `div` 3
        x2 = 2 * x1
        y1 = nrRows `div` 3
        y2 = 2 * y1
        go maxK maxCount maxState state@State {robots} n
            | n > maxIter = (maxK, maxState)
            | otherwise =
                let coords = map position robots
                    middleRobots = length $ filter (\(Coord x y) -> x1 <= x && x <= x2 && y1 <= y && y <= y2) coords
                    state' = moveAllRobots state
                in if middleRobots > maxCount then go n middleRobots state state' (n + 1)
                else go maxK maxCount maxState state' (n + 1)
    in go (-1) (-1) initialState initialState 0


main :: IO ()
main = do
    content <- lines <$> getContents
    let nrRows = 103
    let nrCols = 101
    let initialState = State {robots = parseInput content, nrRows, nrCols}
    let (seconds, robotConfig) = detectXMasTreeConfiguration initialState
    print robotConfig
    print seconds
