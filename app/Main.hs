module Main where

import Graph (Arc(..), Graph(..));


main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    -- 1 --10--> 2
    -- 1 -- 7--> 3
    -- 2 -- 5--> 3
    let graph = Graph [Arc 1 10 2, Arc 1 7 3, Arc 2 5 3]
    print graph
    return ()
