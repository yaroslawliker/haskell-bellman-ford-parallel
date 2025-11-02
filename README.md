## Overview
There are two implementations Bellman-Ford algorythm for searching the
shortest paths in a oriented graph: serial and parallel, written in Haskell.

The algorythm search for shortest path from the starting node to all
other nodes of the graph.

This project is made a the second lab of Functional Programming university course.

## Get started

### Set up
*You may skip this part*

Before building you may want to go to app/Main.hs and change the variable 
`procN = 4` to a number of threads amoung wich you want to split processing.

Also, in app/Main.hs there are few examples like 
```runCompairing 300 procN randomGenerator``` 
where `300` is number of graph nodes (vertices) to generate. You can specify your own
amount of nodes by changing this number to anything you like. You also can
the examples or add the one's you'd like.

Amount of arcs (edges) of the graph is calculated like nodeN * nodeN / 4.

### Build
For building you will need to have Haskel and Cabal installed.

Build with
```cabal build```

Build is already set up to have -threaded option in lab2.cabal.

### Running
Run the following program to run the program with 4 processors:
```cabal run lab2 -- +RTS -N4```

You can change `-N4` to, for example, `-N2` or `-N8` to run, respectively, `2` or `8` processes.
If you do so, make sure to read Set up section of this README first.
