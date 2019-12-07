module Intcode where

import Control.Applicative
import Control.Monad
import Data.List

type Program = [Int]
type Output = [Int]
data Mode = Position | Immediate deriving (Enum)
newtype Counter = Counter Int
data Parameter = Parameter Int Int
type OperationBody = [Parameter] -> Program -> Int -> OperationResult
type Operation = Counter -> [Mode] -> Program -> Int -> [OperationResult]
data OperationResult = NewProgram Program | NewCounter Counter | NewOutput Int | NoResult
type RunState = (Counter, Program, Output)
type State = Either Output RunState
type InputState = Either Output (Int, RunState)

collectResults :: [State] -> [Output]
collectResults [] = []
collectResults (x:xs) = case x of
  Right (_, _, output) -> output : collectResults xs
  Left output -> [output]

addToEither :: c -> Either a b -> Either a (c, b)
addToEither y = fmap (\ x -> (y, x))

resetOutput :: RunState -> RunState
resetOutput (counter, program, _) = (counter, program, [])

orElse :: Maybe a -> a -> a
orElse Nothing value = value
orElse (Just value) _ = value

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
makeOperation n f counter modes program input = results
  where
    parameters = getParameters counter n modes program
    result = f parameters program input
    defaultCounter = NewCounter $ counterAdd counter (n + 1)
    results = case result of
      NoResult     -> [defaultCounter]
      NewCounter _ -> [result]
      _            -> [defaultCounter, result]

add :: OperationBody
add [Parameter _ i, Parameter _ j, k] program _ = NewProgram $ saveValue k (i + j) program

mult :: OperationBody
mult [Parameter _ i, Parameter _ j, k] program _ = NewProgram $ saveValue k (i * j) program

save :: OperationBody
save [i] program input = NewProgram $ saveValue i input program

output :: OperationBody
output [Parameter _ value] program _ = NewOutput value

jumpIfTrue :: OperationBody
jumpIfTrue [Parameter _ i, Parameter _ j] _ _ = if i /= 0 then NewCounter (Counter j) else NoResult

jumpIfFalse :: OperationBody
jumpIfFalse [Parameter _ i, Parameter _ j] _ _ = if 1 == 0 then NewCounter (Counter j) else NoResult

lessThan :: OperationBody
lessThan [Parameter _ i, Parameter _ j, k] program _ = NewProgram $ saveValue k (if i < j then 1 else 0) program

equals :: OperationBody
equals [Parameter _ i, Parameter _ j, k] program _ = NewProgram $ saveValue k (if i == j then 1 else 0) program

addOp :: Operation
addOp = makeOperation 3 add

multOp :: Operation
multOp = makeOperation 3 mult

saveOp :: Operation
saveOp = makeOperation 1 save

outputOp :: Operation
outputOp = makeOperation 1 output

jumpIfTrueOp :: Operation
jumpIfTrueOp = makeOperation 2 jumpIfTrue

jumpIfFalseOp :: Operation
jumpIfFalseOp = makeOperation 2 jumpIfFalse

lessThanOp :: Operation
lessThanOp = makeOperation 3 lessThan

equalsOp :: Operation
equalsOp = makeOperation 3 equals

applyResult :: RunState -> OperationResult -> RunState
applyResult (counter, program, output) result = case result of
  NewProgram newProgram -> (counter, newProgram, output)
  NewCounter newCounter -> (newCounter, program, output)
  NewOutput value       -> (counter, program, value : output)

step :: RunState -> RunState
step state@(counter, program, output) = foldl' applyResult state results
  where
    (opcode, modes) = getOpcodeAndModes counter program
    operation = case opcode of
      1 -> addOp
      2 -> multOp
      4 -> outputOp
      5 -> jumpIfTrueOp
      6 -> jumpIfFalseOp
      7 -> lessThanOp
      8 -> equalsOp
    results = operation counter modes program 0

runUntilInput :: RunState -> State
runUntilInput state@(counter, program, output) =
  let (opcode, _) = getOpcodeAndModes counter program
  in case opcode of
    99 -> Left output
    3 -> Right state
    _ -> runUntilInput $ step state

runWithOneInput :: InputState -> State
runWithOneInput (Left output) = Left output
runWithOneInput (Right (input, state@(counter, program, _))) = runUntilInput $ resetOutput nextState
  where
    (_, modes) = getOpcodeAndModes counter program
    nextState = foldl' applyResult state $ saveOp counter modes program input

runProgram :: Program -> [Int] -> Output
runProgram program input =
  let counter = Counter 0
      (opcode, _) = getOpcodeAndModes counter program
      initialState = if opcode == 3 then Right (counter, program, []) else runUntilInput (counter, program, [])
      results = initialState : map runWithOneInput inputs
      inputs = zipWith addToEither input results
  in join $ collectResults results
