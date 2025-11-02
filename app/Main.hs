module Main where

import System.Random (mkStdGen)

import RunCompairingsIO (runCompairing)

main :: IO ()
main = do
    putStrLn "Graph"

    let randomGenerator = mkStdGen 17
    let procN = 4

    runCompairing 10 procN randomGenerator
    runCompairing 100 procN randomGenerator
    runCompairing 200 procN randomGenerator
    runCompairing 300 procN randomGenerator

    return ()
