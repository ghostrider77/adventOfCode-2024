import Data.Bits (xor)
import Data.List.Split (splitOn)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

data Operand = LiteralOperand Int | ComboOperand Int
data Registers = Registers { regA :: Int, regB :: Int, regC :: Int }
data Operator =
      Adv Operand
    | Bxl Operand
    | Bst Operand
    | Jnz Operand
    | Bxc Operand
    | Out Operand
    | Bdv Operand
    | Cdv Operand
data State = State { program :: Vector Int, pointer :: Int, registers :: Registers, output :: Maybe Int }


parseInput :: [String] -> State
parseInput content =
    let readRegister line = case splitOn ":" line of
            [_, v] -> read v
            _ -> error "Malformed input."
        registers = case map readRegister $ take 3 content of
            [_, b, c] -> Registers {regA = 0, regB = b, regC = c}
            _ -> error "Malformed input."
        program = map read $ splitOn "," $ drop 8 $ content !! 4
    in State {program = V.fromList program, pointer = 0, registers, output = Nothing}


parseOpcode :: Int -> Int -> Operator
parseOpcode 0 k = Adv (ComboOperand k)
parseOpcode 1 k = Bxl (LiteralOperand k)
parseOpcode 2 k = Bst (ComboOperand k)
parseOpcode 3 k = Jnz (LiteralOperand k)
parseOpcode 4 k = Bxc (LiteralOperand k)
parseOpcode 5 k = Out (ComboOperand k)
parseOpcode 6 k = Bdv (ComboOperand k)
parseOpcode 7 k = Cdv (ComboOperand k)
parseOpcode n _ = error ("Unknown opcode " ++ show n)


getOperandValue :: Operand -> Registers -> Int
getOperandValue (LiteralOperand n) _ = n
getOperandValue (ComboOperand n) Registers {regA, regB, regC}
    | 0 <= n && n <= 3 = n
    | n == 4 = regA
    | n == 5 = regB
    | n == 6 = regC
    | otherwise = error ("Unknown combo operand code " ++ show n)


performOperation :: State -> Operator -> State
performOperation state@State {pointer, registers = regs@Registers {regA, regB, regC}, output} operator =
    case operator of
        Adv operand ->
            let value = getOperandValue operand regs
                result = regA `div` (2 ^ value)
            in state {pointer = pointer + 2, registers = regs {regA = result}}
        Bxl operand ->
            let value = getOperandValue operand regs
                result = regB `xor` value
            in state {pointer = pointer + 2, registers = regs {regB = result}}
        Bst operand ->
            let value = getOperandValue operand regs
                result = value `mod` 8
            in state {pointer = pointer + 2, registers = regs {regB = result}}
        Jnz operand ->
            let value = getOperandValue operand regs
            in if regA == 0 then state {pointer = pointer + 2} else state {pointer = value}
        Bxc _ -> state {pointer = pointer + 2, registers = regs {regB = regB `xor` regC}}
        Out operand ->
            let value = getOperandValue operand regs
                result = value `mod` 8
            in state {pointer = pointer + 2, output = Just result}
        Bdv operand ->
            let value = getOperandValue operand regs
                result = regA `div` (2 ^ value)
            in state {pointer = pointer + 2, registers = regs {regB = result}}
        Cdv operand ->
            let value = getOperandValue operand regs
                result = regA `div` (2 ^ value)
            in state {pointer = pointer + 2, registers = regs {regC = result}}


isRegisterValueSuitable :: Int -> State -> Bool
isRegisterValueSuitable targetOutput initialState =
    let go state@State {program, pointer, output} = case output of
            Just result -> result == targetOutput
            Nothing -> case (program !? pointer, program !? (pointer + 1)) of
                (Just opcode, Just k) ->
                    let operator = parseOpcode opcode k
                        state' = performOperation state operator
                    in go state'
                _ -> False
    in go initialState


getSmallestSuitableRegistryValue :: State -> Int
getSmallestSuitableRegistryValue initialState =
    let ps = reverse $ V.toList (program initialState)
        generateStates state@State {registers = regs@Registers {regA}} =
            map (\k -> state {registers = regs {regA = regA * 8 + k}}) [0..7]
        go [] [] = error "No solution has been found."
        go currentStates [] = minimum $ map (regA . registers) currentStates
        go currentStates (target : rest) =
            let nextStates = concatMap (filter (isRegisterValueSuitable target) . generateStates) currentStates
            in go nextStates rest
    in go [initialState] ps


main :: IO ()
main = do
    content <- lines <$> getContents
    let initialState = parseInput content
    print $ getSmallestSuitableRegistryValue initialState
