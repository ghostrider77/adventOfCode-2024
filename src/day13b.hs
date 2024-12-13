import Data.List.Split (splitOn)

data Coord = Coord Int Int deriving (Eq, Show)
data ButtonLabel = A | B deriving (Eq, Show)
data Button = Button { label :: ButtonLabel, xDir :: Int, yDir :: Int, cost :: Int } deriving Show
data Configuration = Configuration { button1 :: Button, button2 :: Button, prizeCoord :: Coord } deriving Show


readLabel :: String -> ButtonLabel
readLabel "A" = A
readLabel "B" = B
readLabel _ = error "Unknown button label."


parseInput :: [String] -> [Configuration]
parseInput contents =
    let bonus = 10000000000000
        getCost bl = if bl == A then 3 else 1
        parseButton l = case words $ filter (\c -> c /= ':' && c /= ',') l of
            [_, b, x, y] -> let label = readLabel b in Button label (read $ drop 2 x) (read $ drop 2 y) (getCost label)
            _ -> error "Malformed button input."
        parsePrize l = case words $ filter (/= ',') l of
            [_, x, y] -> Coord (bonus + read (drop 2 x)) (bonus + read (drop 2 y))
            _ -> error "Malformed prize input."
        go acc ls = case span (/= "") ls of
            ([l1, l2, l3], rest) ->
                let conf = Configuration (parseButton l1) (parseButton l2) (parsePrize l3)
                in go (conf : acc) (if null rest then [] else tail rest)
            (_, []) -> reverse acc
    in go [] contents


solveConfiguration :: Configuration -> Int
solveConfiguration Configuration {button1, button2, prizeCoord = Coord px py} =
    let Button {label = lbl1, xDir = x1, yDir = y1, cost = cost1} = button1
        Button {label = lbl2, xDir = x2, yDir = y2, cost = cost2} = button2
        det = x1 * y2 - y1 * x2
        n1 = px * y2 - x2 * py
        n2 = x1 * py - y1 * px
    in if det == 0 || (n1 `mod` det) /= 0 || (n2 `mod` det) /= 0 then 0
    else cost1 * (n1 `div` det) + cost2 * (n2 `div` det)


calcTokenSum :: [Configuration] -> Int
calcTokenSum configs = sum $ map solveConfiguration configs


main :: IO ()
main = do
    content <- lines <$> getContents
    let configs = parseInput content
    print $ calcTokenSum configs
