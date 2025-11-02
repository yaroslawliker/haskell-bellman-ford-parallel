## Overview
There are two implementations of Bellman-Ford algorythm for searching the
shortest paths in a oriented graph: serial and parallel, written in Haskell.

There is also a convinient way to generate random graph and compare the
algorythm's speed on different processors number.

The algorythm search for shortest path from the starting node to all
other nodes of the graph.

This project is made a the second lab of Functional Programming university course.

## Get started

### Set up
*You may skip this part*

Before building you may want to open app/Main.hs and change the variable 
`procN = 4` to a number of threads amoung which you want to split processing.

Also, in app/Main.hs there are few examples like 
```
runCompairing 300 procN randomGenerator
``` 
where `300` is number of graph nodes (vertices) to generate. You can specify your own
amount of nodes by changing this number to anything you like. You also can
remove the examples or add the one's you'd like.

Amount of arcs (edges) of the graph is calculated as nodeN * nodeN / 4.

### Build
For building you will need to have Haskel and Cabal installed.

Build with:
```
cabal build
```

Build is already set up to have -threaded option in lab2.cabal.

### Running
Run the following program to run the program with 4 processors:
```
cabal run lab2 -- +RTS -N4
```

You can change `-N4` to, for example, `-N2` or `-N8` to run, respectively, `2` or `8` processes.
If you do so, make sure to read Set up section of this README first.

## Comparations
Notations:
* NodeN - number of nodes (vertices)
* ArcN - number of arcs (edges)
* genDuration - time duration of the graph generation
* speedUp = serialDuration / parallelDuration

### 2 processors

| NodeN | arcN | genDuration | serialDuration | parallelDuration | speedUp |
| ---: | ---: | ---:| ---:| ---:| ---:|
| 10  | 25 | 0.000016797s | 0.000116295s | 0.000173031s | 0.67 |
|**100**| 2500 | 0.022714981s | 0.177460785s | 0.104172149s | **1.7** |
| 200 | 10000 | 0.182797555s | 1.38865462s | 1.262424526s | 1.1  |
| 300 | 22500 | 1.073607959s | 7.047307485s | 6.488755224s | 1.09 |

### 4 processors
| NodeN | arcN | genDuration | serialDuration | parallelDuration | speedUp |
| ---: | ---: | ---:| ---:| ---:| ---:|
| 10 | 25 | 0.000016648s | 0.000110508s | 0.000263431s | 0.42 |  
| **100** | 2500 | 0.022499935s | 0.178673851s | 0.076023177s | **2.35** |  
| 200 | 10000 | 0.198677837s | 1.437304164s | 0.9818702s | 1.46 | 
| 300 | 22500 | 1.094372539s | 7.044421813s | 3.969875722s | 1.77 |

### 8 processors
| NodeN | arcN | genDuration | serialDuration | parallelDuration | speedUp |
| ---: | ---: | ---:| ---:| ---:| ---:|
| 10 | 25 | 0.000023518s | 0.000196909s | 0.000841607s | 0.23 |  
| **100** | 2500 | 0.025328352s | 0.201793377s | 0.094078853s | **2.15** |  
| 200 | 10000 | 0.227817092s | 1.666070313s | 0.992109912s | 1.68 |  
| 300 | 22500 | 1.188429897s | 7.467625416s | 3.748199342s | 1.99 |  

### Conclusion
For all experiments the speedUp is bigger on medium data (100 nodes).
The biggeest speedUp is with use of 4 processors on medium data.


