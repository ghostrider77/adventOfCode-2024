import Data.Char (digitToInt)
import Data.List.Extra (minimumOn)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding (Left, Right)

type Layout = Map Coord Button
data Keypad = Numerical Layout | Directional Layout
data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Button = Numeric Int | Activate | Up | Right | Down | Left deriving (Eq, Ord)

data Distance = Dist Int | Infinity deriving Eq
data State = State { queue :: Set (Distance, Coord)
                   , distances :: Map Coord Distance
                   , finalizedCoords :: Set Coord
                   , backtrack :: Map Coord (Set Coord) }


instance Show Button where
    show :: Button -> String
    show Activate = "A"
    show Up = "^"
    show Right = ">"
    show Down = "v"
    show Left = "<"
    show (Numeric k) = show k


instance Ord Distance where
    (<=) :: Distance -> Distance -> Bool
    Infinity <= Infinity = True
    Dist d1 <= Dist d2 = d1 <= d2
    Infinity <= Dist _ = False
    Dist _ <= Infinity = True


addDist :: Distance -> Distance -> Distance
addDist (Dist d1) (Dist d2) = Dist (d1 + d2)
addDist _ _ = Infinity


readButton :: Char -> Button
readButton 'A' = Activate
readButton '^' = Up
readButton '>' = Right
readButton 'v' = Down
readButton '<' = Left
readButton c = Numeric $ digitToInt c


numericKeypad :: Keypad
numericKeypad =
    let coords = [
            (Coord 0 0, Numeric 7), (Coord 0 1, Numeric 8), (Coord 0 2, Numeric 9),
            (Coord 1 0, Numeric 4), (Coord 1 1, Numeric 5), (Coord 1 2, Numeric 6),
            (Coord 2 0, Numeric 1), (Coord 2 1, Numeric 2), (Coord 2 2, Numeric 3),
                                    (Coord 3 1, Numeric 0), (Coord 3 2, Activate)
            ]
    in Numerical $ M.fromList coords


directionalKeypad :: Keypad
directionalKeypad =
    let coords = [(Coord 0 1, Up), (Coord 0 2, Activate), (Coord 1 0, Left), (Coord 1 1, Down), (Coord 1 2, Right)]
    in Directional $ M.fromList coords


parseInput :: [String] -> [[Button]]
parseInput = map (map readButton)


getLayout :: Keypad -> Layout
getLayout (Directional layout) = layout
getLayout (Numerical layout) = layout


codeToInt :: [Button] -> Int
codeToInt code =
    let go acc ((Numeric v) : rest) = go (10 * acc + v) rest
        go acc (Activate : _) = acc
        go acc _ = error "Unknown button series."
    in go 0 code


getNeighbors :: Keypad -> Coord -> [Coord]
getNeighbors keypad (Coord x y) =
    let layout = getLayout keypad
        neighbors = [Coord (x - 1) y, Coord x (y + 1), Coord (x + 1) y, Coord x (y - 1)]
    in filter (`M.member` layout) neighbors


retrieveShortestPaths :: Layout -> Coord -> Map Coord (Set Coord) -> Map (Coord, Coord) [[Coord]]
retrieveShortestPaths layout startCoord backtrack =
    let startButton = layout ! startCoord
        go paths targetCoord
            | null paths = go [[targetCoord]] targetCoord
            | any (\p -> head p == startCoord) paths = ((startCoord, targetCoord), paths)
            | otherwise =
                let paths' = concatMap (\path -> let ps = S.toList $ backtrack ! head path in map (: path) ps) paths
                in go paths' targetCoord
    in M.fromList $ map (go []) $ M.keys layout


updateDistances :: State -> Coord -> Distance -> [Coord] -> State
updateDistances state coord dist neighbors =
    let alterFunc c Nothing = Just $ S.singleton c
        alterFunc c (Just cs) = Just $ S.insert c cs
        go currentState [] = currentState
        go currentState@State {queue, distances, backtrack} (n : ns) =
            let distanceThroughNode = addDist dist (Dist 1)
                currentDistance = M.findWithDefault Infinity n distances
            in if currentDistance < distanceThroughNode then go currentState ns
            else if currentDistance == distanceThroughNode then
                let backtrack' = M.alter (alterFunc coord) n backtrack
                in go currentState {backtrack = backtrack'} ns
            else
                let queue' = S.insert (distanceThroughNode, n) queue
                    distances' = M.insert n distanceThroughNode distances
                    backtrack' = M.alter (alterFunc coord) n backtrack
                in go currentState {queue = queue', distances = distances', backtrack = backtrack'} ns
    in go state neighbors


convertCoordsToPath :: [Coord] -> [String]
convertCoordsToPath ((Coord x1 y1) : c2@(Coord x2 y2) : rest)
    | x2 == x1 - 1 = show Up : convertCoordsToPath (c2 : rest)
    | y2 == y1 + 1 = show Right : convertCoordsToPath (c2 : rest)
    | x2 == x1 + 1 = show Down : convertCoordsToPath (c2 : rest)
    | y2 == y1 - 1 = show Left : convertCoordsToPath (c2 : rest)
convertCoordsToPath _ = [show Activate]


calcShortestPaths :: Keypad -> Map (Button, Button) [String]
calcShortestPaths keypad =
    let layout = getLayout keypad
        coords = M.keys layout
        go state@State {queue, distances, finalizedCoords, backtrack} =
            case S.lookupMin queue of
                Nothing ->
                    let (startCoord, _) = minimumOn snd $ M.assocs distances
                    in retrieveShortestPaths layout startCoord backtrack
                Just (dist, coord) ->
                    if S.member coord finalizedCoords then go state {queue = S.deleteMin queue}
                    else
                        let neighbors = getNeighbors keypad coord
                            state' = updateDistances state coord dist neighbors
                        in go state' { finalizedCoords = S.insert coord finalizedCoords }
        ms = M.unions $ map (\c -> go $ State (S.singleton (Dist 0, c)) (M.singleton c (Dist 0)) S.empty M.empty) coords
        ms' = map (\((c1, c2), ps) -> ((layout ! c1, layout ! c2), map (concat . convertCoordsToPath) ps)) $ M.assocs ms
    in M.fromList ms'


solveNumericalPad :: [[Button]] -> [[String]]
solveNumericalPad codes =
    let shortestPaths = calcShortestPaths numericKeypad
        go acc [] = map (concat . reverse) acc
        go acc ((a, b) : rest) =
            let paths = shortestPaths ! (a, b)
                acc' = concatMap (\p -> map (p :) acc) paths
            in go acc' rest
    in map (\code -> go [[]] $ zip (Activate : code) code) codes


calcCodeComplexities :: [[Button]] -> Int -> Int
calcCodeComplexities codes nrRobots =
    let shortestNumericalPaths = solveNumericalPad codes
        shortestDirectionalPaths = calcShortestPaths directionalKeypad
        directionalPathLengths = M.map (length . head) shortestDirectionalPaths
        codeWithPaths = zip codes shortestNumericalPaths
        go code n cache
            | n == 1 = (sum $ zipWith (curry (directionalPathLengths !)) (Activate : code) code, cache)
            | otherwise = case M.lookup (code, n) cache of
                Just result -> (result, cache)
                Nothing ->
                    let codePairs = zip (Activate : code) code
                        process (acc, chc) pair =
                            let paths = shortestDirectionalPaths ! pair
                                (r, cache') = foldl (\(m, c) p ->
                                    let (res, c') = go (map readButton p) (n - 1) c
                                    in (min m res, M.insert (map readButton p, n - 1) res c')) (maxBound, chc) paths
                            in (acc + r, cache')
                    in foldl process (0, cache) codePairs
    in foldl (\acc (code, paths) ->
        acc + codeToInt code * minimum (map (\p -> fst (go (map readButton p) nrRobots M.empty)) paths)) 0 codeWithPaths


main :: IO ()
main = do
    content <- lines <$> getContents
    let codes = parseInput content
    print $ calcCodeComplexities codes 25
