module Main where

import Data.List

import Criterion.Main
import System.Process hiding (ProcessHandle)
import System.Environment
import Control.Monad
import Control.DeepSeq
import Criterion.Types

import System.Win32.Types
import System.Win32.Thread
import System.Win32.Process
import Foreign.C.Types
import Foreign.Ptr




foreign import ccall  "windows.h SetThreadAffinityMask"
    c_SetThreadAffinityMask :: THANDLE -> DDWORD -> IO DWORD_PTR

foreign import ccall  "windows.h SetPriorityClass"
    c_SetPriorityClass :: ProcessHandle -> DWORD -> IO CBool

{-

BOOL WINAPI SetPriorityClass(
  _In_ HANDLE hProcess,
  _In_ DWORD  dwPriorityClass
);


DWORD_PTR WINAPI SetThreadAffinityMask(
  _In_ HANDLE    hThread,
  _In_ DWORD_PTR dwThreadAffinityMask
);

BOOL WINAPI SetThreadPriority(
  _In_ HANDLE hThread,
  _In_ int    nPriority
); -}

thread_mask :: DDWORD
thread_mask = 4

setupEnv :: IO ()
setupEnv = do
  th <- getCurrentThread
  ph <- getCurrentProcess
  c_SetPriorityClass ph 0x00000080 -- HIGH_PRIORITY_CLASS
  --print =<< c_SetThreadAffinityMask th thread_mask

  return ()


executeFile :: FilePath -> [String] -> IO String
executeFile path args =
  readProcess path args []

benchConfig :: Config
benchConfig =
  defaultConfig
    { timeLimit = 3
    }

splitArgs :: [String] -> ([[String]])
splitArgs [] = []
splitArgs args =
  let (a1, remainingArgs) = span (/= "--") args
  in
  a1:(if null remainingArgs then  [] else splitArgs (tail remainingArgs))

createBenchmark :: [String] -> Benchmark
createBenchmark x@(file:parameters) =
  bench ("execute: " ++ unwords x) $ nfIO (executeFile file parameters)

main :: IO ()
main = do
  setupEnv
  args <- getArgs
  let (bargs, critArgs') = span ( /= "--CRIT") args
  let critArgs = if null critArgs' then [] else tail critArgs'
  let execArgs = splitArgs bargs
  when (null execArgs) $ fail "No executable given"
  deepseq execArgs $ return ()
  let benchmarks = map createBenchmark execArgs
  withArgs (critArgs) $ defaultMainWith benchConfig benchmarks

