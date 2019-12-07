module Intcode where

import Control.Applicative
import Control.Monad

type Program = [Int]
type Input = [Int]
type Output = [Int]
data Mode = Position | Immediate deriving (Enum)
newtype Counter = Counter Int
data Parameter = Parameter Int Int
type OperationBody = [Parameter] -> Program -> Input -> Output -> PartialState
type Operation = Counter -> [Mode] -> Program -> Input -> Output -> State
data State = Run Counter Program Input Output | Stop Int Output
data PartialState = PartialState Program Input Output
data Result = Result Int Output deriving (Show)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 val (x:xs) = val:xs
replaceNth i val (x:xs) = x : replaceNth (i - 1) val xs

saveValue :: Parameter -> Int -> Program -> Program
saveValue (Parameter index _) = replaceNth index

getModes :: Int -> [Mode]
getModes 0 = repeat Position
getModes i = toEnum (i `mod` 10) : getModes (i `div` 10)

getOpcodeAndModes :: Counter -> Program -> (Int, [Mode])
getOpcodeAndModes (Counter counter) program = (opcode, modes)
  where
    i = program !! counter
    opcode = i `mod` 100
    modes = getModes $ i `div` 100

getParameter :: Program -> (Mode, Int) -> Parameter
getParameter program (mode, n) = Parameter n value
  where
    value = case mode of
      Position -> program !! n
      Immediate -> n

getParameters :: Counter -> Int -> [Mode] -> Program -> [Parameter]
getParameters (Counter counter) n modes program = getParameter program <$> zip modes values
  where values = take n $ drop (counter + 1) program

counterAdd :: Counter -> Int -> Counter
counterAdd (Counter counter) n = Counter $ counter + n

makeOperation :: Int -> OperationBody -> Operation
makeOperation n f counter modes program input output = Run (counterAdd counter (n + 1)) newProgram newInput newOutput
  where
    parameters = getParameters counter n modes program
    (PartialState newProgram newInput newOutput) = f parameters program input output

add :: OperationBody
add [Parameter _ i, Parameter _ j, k] program = PartialState newProgram
  where newProgram = saveValue k (i + j) program

mult :: OperationBody
mult [Parameter _ i, Parameter _ j, k] program = PartialState newProgram
  where newProgram = saveValue k (i * j) program

save :: OperationBody
save [i] program (input:inputs) = PartialState newProgram inputs
  where newProgram = saveValue i input program

output :: OperationBody
output [Parameter _ value] program inputs outputs = PartialState program inputs (value : outputs)

addOp :: Operation
addOp = makeOperation 3 add

multOp :: Operation
multOp = makeOperation 3 mult

saveOp :: Operation
saveOp = makeOperation 1 save

outputOp :: Operation
outputOp = makeOperation 1 output

step :: Counter -> Program -> Input -> Output -> State
step counter program inputs outputs = 
  if opcode == 99
    then Stop (head program) outputs
    else operation counter modes program inputs outputs
  where
    (opcode, modes) = getOpcodeAndModes counter program
    operation = case opcode of
      1 -> addOp
      2 -> multOp
      3 -> saveOp
      4 -> outputOp

doRun :: State -> Result
doRun (Run counter program input output) = doRun $ step counter program input output
doRun (Stop value output) = Result value (reverse output)

runProgram :: Program -> Input -> Result
runProgram program input = doRun $ Run (Counter 0) program input []
