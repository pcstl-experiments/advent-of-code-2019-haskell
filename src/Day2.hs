module Day2
  ( day2Main
  ) where

import Data.Array          (Array, (//), listArray, (!))
import Data.Maybe          (fromJust)
import Control.Monad.State (State, state, get, execState)
import Control.DeepSeq     (deepseq)
import System.IO           (getContents)

type Program = Array Int Int
type Args = Maybe (Int, Int, Int)

tokenize :: String -> [Int]
tokenize = map read . split ','

parse :: [Int] -> Program
parse l = listArray (0, (length l)-1) l

split :: Char -> String -> [String]
split _     "" = []
split delim s  =
  let (prefix, rest) = break (== delim) s in
    if null rest
    then [prefix]
    else prefix:(split delim (tail rest))

data InterpreterState = IState { runningProgram  :: Program
                               , currentPosition :: Int
                               , shouldHalt      :: Bool
                               } deriving Show

initialInterpreterState :: Program -> InterpreterState
initialInterpreterState program = IState program 0 False

runInstructions :: State InterpreterState ()
runInstructions = do
  runInstruction
  currentState <- get
  if shouldHalt currentState
    then return ()
    else runInstructions

runInstruction :: State InterpreterState ()
runInstruction = do
  opcode <- getOpcode
  args   <- getArgs opcode
  runOpcode opcode args
  stepPosition

stepPosition :: State InterpreterState ()
stepPosition = state stepPosition'
  where stepPosition' (IState prog pos h) = ((), IState prog (pos+4) h)

getOpcode :: State InterpreterState Int
getOpcode = state getOpcode'
  where getOpcode' s@(IState prog pos _) = (prog ! pos, s)

getArgs :: Int -> State InterpreterState Args
getArgs opcode = state getArgs'
  where getArgs' s@(IState prog pos _)
          | opcode == 99 = (Nothing, s)
          | otherwise    =
            let leftIndex  = prog ! (pos + 1)
                rightIndex = prog ! (pos + 2)
                outIndex   = prog ! (pos + 3)
                leftArg    = prog ! leftIndex
                rightArg   = prog ! rightIndex
            in (Just (leftArg, rightArg, outIndex), s)

runOpcode :: Int
  -> Args
  -> State InterpreterState ()
runOpcode n args
  | n == 1    = state $ runFun (+) (fromJust args)
  | n == 2    = state $ runFun (*) (fromJust args)
  | n == 99   = state halt
  | otherwise = error $ "Invalid opcode " ++ (show n)

halt :: InterpreterState -> ((), InterpreterState)
halt (IState prog pos _) = ((), IState prog pos True)

runFun :: (Int -> Int -> Int)
  -> (Int, Int, Int)
  -> InterpreterState
  -> ((), InterpreterState)
runFun fun (l, r, out) (IState prog pos h) =
  let newProg = prog // [(out, fun l r)] in ((), IState newProg pos h)

patchProgram :: Int -> Int -> Program -> Program
patchProgram x y program  =
  program // [(1, x), (2, y)]

patchAndRun :: Int -> Int -> Program -> InterpreterState
patchAndRun x y program =
  let patchedProgram = patchProgram x y program
      initialState   = initialInterpreterState patchedProgram
  in execState runInstructions initialState

testParams :: Program -> Int -> Int -> Int
testParams program x y =
  (runningProgram $ patchAndRun x y program) ! 0

target = 19690720

bruteForce :: Program -> (Int, Int)
bruteForce prog = bruteForce' prog 0 0
  where bruteForce' p x y
          | x >  99 && y <= 99 = bruteForce' p 0 (y+1)
          | x <= 99 && y <= 99 =
            if testParams p x y == target
            then (x,y)
            else bruteForce' p (x+1) y
          | otherwise = error "Possibilities exhausted."

getAnswer :: (Int, Int) -> Int
getAnswer (noun, verb) = 100 * noun + verb

getProgram :: IO Program
getProgram = do
  input <- getContents
  input `deepseq` (return $ (parse . tokenize) input)

day2Main :: IO ()
day2Main = do
  program <- getProgram
  putStrLn (show $ (getAnswer . bruteForce) program)
