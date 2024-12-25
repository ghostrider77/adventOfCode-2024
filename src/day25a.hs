import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as S

data Device = Lock | Key deriving (Eq, Ord, Show)
data Schema = Schema { device :: Device, pinHeights :: [Int] } deriving (Eq, Ord, Show)


calcPinHeights :: [String] -> Device -> [Int]
calcPinHeights ls device = case device of
    Lock ->
        let rows = transpose $ tail ls
        in map (length . takeWhile (== '#')) rows
    Key ->
        let rows = transpose $ init ls
        in map (length . dropWhile (== '.')) rows


parseInput :: [String] -> [Schema]
parseInput content =
    let go acc [] = reverse acc
        go acc ls =
            let (s, rest) = span (/= "") ls
                device
                    | all (== '#') $ last s = Key
                    | all (== '#') $ head s = Lock
                    | otherwise = error "Malformed input"
                pinHeights = calcPinHeights s device
                rest' = if null rest then [] else tail rest
            in go (Schema {device, pinHeights} : acc) rest'
    in go [] content


fittingSchemas :: Schema -> Schema -> Bool
fittingSchemas Schema {device = Lock, pinHeights = h1} Schema {device = Key, pinHeights = h2} =
    all (<= 5) $ zipWith (+) h1 h2
fittingSchemas _ _ = False


getUniqueLockKeyPairs :: [Schema] -> Int
getUniqueLockKeyPairs schemas =
    let locks = S.fromList $ filter (\s -> device s == Lock) schemas
        keys = S.fromList $ filter (\s -> device s == Key) schemas
    in S.foldl (\acc (lock, key) -> if fittingSchemas lock key then acc + 1 else acc) 0 $ S.cartesianProduct locks keys


main :: IO ()
main = do
    content <- lines <$> getContents
    let schematics = parseInput content
    print $ getUniqueLockKeyPairs schematics
