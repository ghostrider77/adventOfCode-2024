import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~), getAllTextMatches)


scanCorruptedMemory :: String -> [(Int, Int)]
scanCorruptedMemory memory =
    let getArgs m = case splitOn "," $ drop 4 $ init m of
            [a, b] -> (read a, read b)
            _ -> error "Malformed input."
        pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
        matches = getAllTextMatches $ memory =~ pattern :: [String]
    in map getArgs matches


multiplyUncorruptedInstructions :: String -> Int
multiplyUncorruptedInstructions memory =
    let arguments = scanCorruptedMemory memory
    in foldl (\acc (a, b) -> acc + a * b) 0 arguments


main :: IO ()
main = do
    content <- getContents
    print $ multiplyUncorruptedInstructions content
