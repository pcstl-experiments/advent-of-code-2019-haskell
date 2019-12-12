module Day2
  ( day2Main
  ) where

import Data.Array          (Array, (//), listArray, (!), elems)
import Data.Maybe          (fromJust)
import Control.Monad.State (State, state, get, execState)
import Control.DeepSeq     (deepseq)
import System.IO           (getContents)
import Debug.Trace

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

patchProgram :: Program -> Program
patchProgram program =
  program // [(1, 12), (2, 2)]

patchAndRun :: Program -> InterpreterState
patchAndRun program =
  let patchedProgram = patchProgram program
      initialState   = initialInterpreterState patchedProgram
  in execState runInstructions initialState

printResult :: [Int] -> IO ()
printResult = putStrLn . show

getProgram :: IO Program
getProgram = do
  input <- getContents
  input `deepseq` (return $ (parse . tokenize) input)

day2Main :: IO ()
day2Main = do
  program <- getProgram
  printResult $ (elems . runningProgram) (patchAndRun program)
