module Main where

import System.Random (mkStdGen)

import RunCompairingsIO (runCompairing)

main :: IO ()
main = do
    putStrLn "Graph"

    let randomGenerator = mkStdGen 17

    r1 <- runCompairing 10 randomGenerator
    r2 <- runCompairing 100 randomGenerator
    r3 <- runCompairing 200 randomGenerator
    r4 <- runCompairing 400 randomGenerator

    return ()
