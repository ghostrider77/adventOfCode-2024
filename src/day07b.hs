import Data.List.Split (splitOn)

data Equation = Equation { testValue :: Integer, terms :: [Integer] }


convertToIntegerList :: String -> [Integer]
convertToIntegerList = map read . words


parseInput :: [String] -> [Equation]
parseInput ls =
    let parseLine line = case splitOn ": " line of
            [test, rest] -> Equation {testValue = read test, terms = convertToIntegerList rest}
            _ -> error "Malformed input."
    in map parseLine ls


canEquationBeSatisfied :: Equation -> Bool
canEquationBeSatisfied (Equation testValue []) = False
canEquationBeSatisfied (Equation testValue [t]) = t == testValue
canEquationBeSatisfied (Equation testValue (t1 : t2 : rest))
    | t1 > testValue || t2 > testValue = False
    | otherwise =
        let added = canEquationBeSatisfied (Equation {testValue, terms = (t1 + t2) : rest})
            multiplied = canEquationBeSatisfied (Equation {testValue, terms = (t1 * t2) : rest})
            concatenated = canEquationBeSatisfied (Equation {testValue, terms = read (show t1 ++ show t2) : rest})
        in added || multiplied || concatenated


calcTotalCalibrationResult :: [Equation] -> Integer
calcTotalCalibrationResult = foldl (\acc e -> if canEquationBeSatisfied e then acc + testValue e else acc) 0


main :: IO ()
main = do
    content <- lines <$> getContents
    let equations = parseInput content
    print $ calcTotalCalibrationResult equations
