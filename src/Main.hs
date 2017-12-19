module Main where

import Criterion.Main
import System.Process
import System.Environment
import Control.Monad
import Control.DeepSeq


executeFile :: FilePath -> [String] -> IO String
executeFile path args =
  readProcess path args []

splitArgs :: [String] -> [[String]]
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
  args <- getArgs
  let execArgs = splitArgs args
  when (null execArgs) $ fail "No executable given"
  deepseq execArgs $ return ()
  let benchmarks = map createBenchmark execArgs
  withArgs ["-v 1"] $ defaultMain benchmarks

