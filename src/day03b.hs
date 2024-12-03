import Data.List.Split (splitOn)
import Text.Regex (mkRegex, matchRegexAll)
import Text.Regex.Posix ((=~), getAllTextMatches)

data State = Enabled | Disabled deriving (Eq, Show)


switchState :: State -> State
switchState Enabled = Disabled
switchState Disabled = Enabled


parseMemory :: String -> [(Int, Int)]
parseMemory memory =
    let getArgs m = case splitOn "," $ drop 4 $ init m of
            [a, b] -> (read a, read b)
            _ -> error "Malformed input."
        pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
        matches = getAllTextMatches $ memory =~ pattern :: [String]
    in map getArgs matches


scanCorruptedMemory :: String -> [(Int, Int)]
scanCorruptedMemory line =
    let enable = mkRegex "do\\(\\)"
        disable = mkRegex "don't\\(\\)"
        go acc _ [] = acc
        go acc state memory =
            let pattern = if state == Enabled then disable else enable
            in case matchRegexAll pattern memory of
                Just (currentPart, _, nextPart, _) ->
                    let arguments = if state == Enabled then parseMemory currentPart else []
                    in go (arguments ++ acc) (switchState state) nextPart
                Nothing -> if state == Enabled then parseMemory memory ++ acc else acc
    in go [] Enabled line


multiplyUncorruptedInstructions :: String -> Int
multiplyUncorruptedInstructions memory =
    let arguments = scanCorruptedMemory memory
    in foldl (\acc (a, b) -> acc + a * b) 0 arguments


main :: IO ()
main = do
    content <- getContents
    print $ multiplyUncorruptedInstructions content
