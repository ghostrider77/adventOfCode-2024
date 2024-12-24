import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

data Node = Node { name :: String, leftWire :: String, rightWire :: String, operator :: Bool -> Bool -> Bool }


readOperation :: String -> (Bool -> Bool -> Bool)
readOperation "AND" = (&&)
readOperation "OR" = (||)
readOperation "XOR" = (/=)


parseInput :: [String] -> (Map String Bool, [Node])
parseInput content =
    let readValue line = case splitOn ": " line of
            [a, b] -> (a, toEnum $ read b)
            _ -> error ("Malformed input " ++ line)
        readNode line = case splitOn " -> " line of
            [a, b] -> case words a of
                [w1, op, w2] -> Node {name = b, leftWire = w1, rightWire = w2, operator = readOperation op}
                _ -> error ("Malformed input " ++ line)
            _ -> error ("Malformed input " ++ line)
        (first, second) = span (/= "") content
        wireValues = M.fromList $ map readValue first
        gates = map readNode (tail second)
    in (wireValues, gates)


collectOutputs :: Map String Bool -> [Bool]
collectOutputs values = map snd $ M.toDescList $ M.filterWithKey (\k _ -> head k == 'z') values


convertToInt :: [Bool] -> Int
convertToInt = foldl (\acc x -> 2 * acc + fromEnum x) 0


evaluateGates :: [Node] -> Map String Bool -> Int
evaluateGates gates values =
    let go [] [] values = let bits = collectOutputs values in convertToInt bits
        go unevaluated [] values = go [] unevaluated values
        go unevaluated (node@Node {name, leftWire, rightWire, operator} : nodes) values =
            case (M.lookup leftWire values, M.lookup rightWire values) of
                (Just l, Just r) ->
                    let v = operator l r
                        values' = M.insert name v values
                    in go unevaluated nodes values'
                _ -> go (node : unevaluated) nodes values
    in go [] gates values


main :: IO ()
main = do
    content <- lines <$> getContents
    let (values, gates) = parseInput content
    print $ evaluateGates gates values
