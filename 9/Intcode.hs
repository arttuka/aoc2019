module Intcode where

import Prelude hiding ((!!), concat, lookup)
import Control.Applicative
import Control.Monad
import Data.Either (either)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!), (!?), (//), concat, slice, fromList, toList)
import qualified Data.Vector as V

type Program = Vector Int
data Mode = Position | Immediate | Relative deriving (Enum)
data Parameter = Parameter Int Int
type InstructionBody = [Parameter] -> Program -> InstructionResult
data Instruction = Instruction Int InstructionBody
data InstructionResult = NewProgram Program | NewPointer Int | NewOutput Int | AdjustBase Int | NoResult
data State = State { pointer :: Int
                   , base    :: Int
                   , program :: Program
                   , outputs :: [Int]
                   }
type Result = Either [Int] State
type Input = (Int, State)

initialRights :: [Either a b] -> [b]
initialRights (Right x : xs) = x : initialRights xs
initialRights _              = []

toOutput :: Result -> [Int]
toOutput = either id outputs

resetOutput :: State -> State
resetOutput state = state { outputs = [] }

extendProgram :: Int -> Int -> Vector Int
extendProgram n value = fromList $ extend n value
  where
    extend 0 value = [value]
    extend k value = 0 : extend (k - 1) value

(!!) :: Program -> Int -> Int
(!!) program n = fromMaybe 0 $ program !? n

getModes :: Int -> Program -> [Mode]
getModes pointer program = nextModes $ (program !! pointer) `div` 100
  where
    nextModes 0 = repeat Position
    nextModes i = toEnum (i `mod` 10) : nextModes (i `div` 10)

getOpcode :: Int -> Program -> Int
getOpcode pointer program = (program !! pointer) `mod` 100

getParameter :: State -> Mode -> Int -> Parameter
getParameter State{program=program, base=base} mode n = case mode of
  Position  -> Parameter n $ program !! n
  Immediate -> Parameter n n
  Relative  -> Parameter (base + n) $ program !! (base + n)

getParameters :: State -> Int -> [Parameter]
getParameters state@State{pointer=pointer, program=program} n = zipWith (getParameter state) modes values
  where
    modes  = getModes pointer program
    values = toList $ slice (pointer + 1) n program

saveValue :: Parameter -> Int -> Program -> InstructionResult
saveValue (Parameter index _) value program
    | index < n = NewProgram $ program // [(index, value)]
    | otherwise = NewProgram $ (V.++) program $ extendProgram (index - n) value
  where
    n = length program

saveInput :: State -> Int -> [InstructionResult]
saveInput state@State{pointer=pointer, program=program} input = results
  where
    [parameter] = getParameters state 1
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

adjustBase :: InstructionBody
adjustBase [Parameter _ value] _ = AdjustBase value

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
  , Instruction 1 adjustBase
  ]

applyResult :: State -> InstructionResult -> State
applyResult state result = case result of
  NewProgram newProgram -> state { program = newProgram }
  NewPointer newPointer -> state { pointer = newPointer }
  NewOutput value       -> state { outputs = outputs state ++ [value] }
  AdjustBase value      -> state { base = base state + value }

step :: State -> State
step state@State{ pointer=pointer, program=program } = foldl' applyResult state results
  where
    opcode      = getOpcode pointer program
    (Instruction n inst) = instructions ! opcode 
    parameters           = getParameters state n
    result               = inst parameters program
    defaultPointer       = NewPointer $ pointer + n + 1
    results              = case result of
      NoResult     -> [defaultPointer]
      NewPointer _ -> [result]
      _            -> [defaultPointer, result]

runUntilInput :: State -> Result
runUntilInput state@State{ pointer=pointer, program=program, outputs=outputs} = case opcode of
    99 -> Left outputs
    3  -> Right state
    _  -> runUntilInput $ step state
  where
    opcode = getOpcode pointer program

continueFromInput :: Input -> Result
continueFromInput (input, state) = runUntilInput $ resetOutput nextState
  where
    nextState = foldl' applyResult state $ saveInput state input

runProgram :: Program -> [Int] -> [Int]
runProgram program input = results >>= toOutput
  where
    initialState = runUntilInput State { pointer = 0, base = 0, program = program, outputs = [] }
    results      = initialState : map continueFromInput inputs
    inputs       = zip input $ initialRights results
