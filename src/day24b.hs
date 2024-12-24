import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data Gate = Gate { name :: String, leftWire :: String, rightWire :: String, operator :: String }


parseInput :: [String] -> (Int, [Gate])
parseInput content =
    let readNode line = case splitOn " -> " line of
            [a, b] -> case words a of
                [w1, op, w2] -> Gate {name = b, leftWire = w1, rightWire = w2, operator = op}
                _ -> error ("Malformed input " ++ line)
            _ -> error ("Malformed input " ++ line)
        (first, second) = span (/= "") content
        gates = map readNode (tail second)
    in (length first `div` 2, gates)


isIntermediateGate :: Gate -> Bool
isIntermediateGate Gate {name, leftWire, rightWire} =
    let finalNodes = ['x', 'y', 'z']
    in head name `notElem` finalNodes && head leftWire `notElem` finalNodes && head rightWire `notElem` finalNodes


collectFalseGates :: [Gate] -> Int -> String
collectFalseGates gates nrBits =
    let addWrongANDGate acc name =
            if any (\g -> (leftWire g == name || rightWire g == name) && operator g /= "OR") gates
                then S.insert name acc
            else acc
        addWrongORGate acc name =
            if any (\g -> (leftWire g == name || rightWire g == name) && operator g == "OR") gates
                then S.insert name acc
            else acc
        go acc [] = intercalate "," $ S.toAscList acc
        go acc (gate@Gate {name, leftWire, rightWire, operator} : gs)
            | (head name == 'z') && operator /= "XOR" && name /= ("z" ++ show nrBits) = go (S.insert name acc) gs
            | operator == "AND" && "x00" /= leftWire && "x00" /= rightWire =
                let acc' = addWrongANDGate acc name in go acc' gs
            | operator == "XOR" =
                if isIntermediateGate gate then go (S.insert name acc) gs
                else let acc' = addWrongORGate acc name in go acc' gs
            | otherwise = go acc gs
    in go S.empty gates


main :: IO ()
main = do
    content <- lines <$> getContents
    let (nrBits, gates) = parseInput content
    putStrLn $ collectFalseGates gates nrBits
