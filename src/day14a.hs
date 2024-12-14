{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Coord = Coord Int Int deriving (Eq, Show)
data Quadrant = TL | TR | BR | BL deriving (Eq, Ord)
data Robot = Robot { position :: Coord, vX :: Int, vY :: Int } deriving Show
data State = State { robots :: [Robot], nrRows :: Int, nrCols :: Int } deriving Show


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


countRobotsInQuadrants :: State -> Map Quadrant Int
countRobotsInQuadrants State {robots, nrRows, nrCols} =
    let getQuadrant (Coord x y)
            | x < nrCols `div` 2 && y < nrRows `div` 2 = Just TL
            | x < nrCols `div` 2 && y > nrRows `div` 2 = Just BL
            | x > nrCols `div` 2 && y < nrRows `div` 2 = Just TR
            | x > nrCols `div` 2 && y > nrRows `div` 2 = Just BR
            | otherwise = Nothing
        quadrants = mapMaybe (\Robot {position} -> getQuadrant position) robots
    in M.fromListWith (+) $ map (, 1) quadrants


calcSafetyFactor :: State -> Int -> Int
calcSafetyFactor state n =
    let state' = iterate moveAllRobots state !! n
        counts = countRobotsInQuadrants state'
    in product $ M.elems counts


main :: IO ()
main = do
    content <- lines <$> getContents
    let nrRows = 103
    let nrCols = 101
    let n = 100
    let initialState = State {robots = parseInput content, nrRows, nrCols}
    print $ calcSafetyFactor initialState n
