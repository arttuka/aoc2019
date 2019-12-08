module Intcode where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.Either (either, isLeft, rights)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//), slice, fromList, toList)

type Program = Vector Int
data Mode = Position | Immediate deriving (Enum)
data Parameter = Parameter Int Int
type InstructionBody = [Parameter] -> Program -> InstructionResult
data Instruction = Instruction Int InstructionBody
data InstructionResult = NewProgram Program | NewPointer Int | NewOutput Int | NoResult
type State = (Int, Program, [Int])
type Result = Either [Int] State
type Input = (Int, State)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []  = []
takeUntil p (x:xs)
    | p x       = [x]
    | otherwise = x : takeUntil p xs
                  
third :: (a, b, c) -> c
third (_, _, c) = c

toOutput :: Result -> [Int]
toOutput = either id third

resetOutput :: State -> State
resetOutput (pointer, program, _) = (pointer, program, [])

getModes :: Int -> [Mode]
getModes 0 = repeat Position
getModes i = toEnum (i `mod` 10) : getModes (i `div` 10)

getOpcodeAndModes :: Int -> Program -> (Int, [Mode])
getOpcodeAndModes pointer program = (opcode, modes)
  where
    i      = program ! pointer
    opcode = i `mod` 100
    modes  = getModes $ i `div` 100

getParameter :: Program -> Mode -> Int -> Parameter
getParameter program mode n = Parameter n value
  where
    value = case mode of
      Position  -> program ! n
      Immediate -> n

getParameters :: Int -> Int -> [Mode] -> Program -> [Parameter]
getParameters pointer n modes program = zipWith (getParameter program) modes values
  where
    values = toList $ slice (pointer + 1) n program

saveValue :: Parameter -> Int -> Program -> InstructionResult
saveValue (Parameter index _) value program = NewProgram $ program // [(index, value)]

saveInput :: Int -> Int -> Program -> [InstructionResult]
saveInput pointer input program = results
  where
    [parameter] = getParameters pointer 1 [Position] program
    results     = [ saveValue parameter input program
                  , NewPointer (pointer + 2)
                  ]

noop = Instruction 0 (\_ _ -> NoResult)

add :: InstructionBody
add [Parameter _ i, Parameter _ j, k] = saveValue k (i + j)

mult :: InstructionBody
mult [Parameter _ i, Parameter _ j, k] = saveValue k (i * j)

output :: InstructionBody
output [Parameter _ value] _ = NewOutput value

jumpIfTrue :: InstructionBody
jumpIfTrue [Parameter _ i, Parameter _ j] _
    | i /= 0    = NewPointer j
    | otherwise = NoResult

jumpIfFalse :: InstructionBody
jumpIfFalse [Parameter _ i, Parameter _ j] _
    | i == 0    = NewPointer j
    | otherwise = NoResult

lessThan :: InstructionBody
lessThan [Parameter _ i, Parameter _ j, k] = saveValue k (if i < j then 1 else 0)

equals :: InstructionBody
equals [Parameter _ i, Parameter _ j, k] = saveValue k (if i == j then 1 else 0)

instructions :: Vector Instruction
instructions = fromList
  [ noop
  , Instruction 3 add
  , Instruction 3 mult
  , noop
  , Instruction 1 output
  , Instruction 2 jumpIfTrue
  , Instruction 2 jumpIfFalse
  , Instruction 3 lessThan
  , Instruction 3 equals
  ]

applyResult :: State -> InstructionResult -> State
applyResult (pointer, program, output) result = case result of
  NewProgram newProgram -> (pointer, newProgram, output)
  NewPointer newPointer -> (newPointer, program, output)
  NewOutput value       -> (pointer, program, output ++ [value])

step :: State -> State
step state@(pointer, program, _) = foldl' applyResult state results
  where
    (opcode, modes)      = getOpcodeAndModes pointer program
    (Instruction n inst) = instructions ! opcode 
    parameters           = getParameters pointer n modes program
    result               = inst parameters program
    defaultPointer       = NewPointer $ pointer + n + 1
    results              = case result of
      NoResult     -> [defaultPointer]
      NewPointer _ -> [result]
      _            -> [defaultPointer, result]

runUntilInput :: State -> Result
runUntilInput state@(pointer, program, output) = case opcode of
    99 -> Left output
    3  -> Right state
    _  -> runUntilInput $ step state
  where
    (opcode, _) = getOpcodeAndModes pointer program

continueFromInput :: Input -> Result
continueFromInput (input, state@(pointer, program, _)) = runUntilInput $ resetOutput nextState
  where
    nextState = foldl' applyResult state $ saveInput pointer input program

runProgram :: Program -> [Int] -> [Int]
runProgram program input = takeUntil isLeft results >>= toOutput
  where
    initialState = runUntilInput (0, program, [])
    results      = initialState : map continueFromInput inputs
    inputs       = zip input $ rights results
