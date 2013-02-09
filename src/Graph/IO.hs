module Graph.IO where

import System.IO
import Graph
import Data.Array.IArray
import Data.List
import Data.List.Split

hReadGraph :: Handle -> IO Graph
hReadGraph h = do
    firstLine <- hGetLine h
    let n:m:[] = map read $ splitOn " " firstLine
    edges <- sequence $ replicate m (hReadEdge h)
    return (Graph [1..n] edges)

hReadEdge :: Handle -> IO Edge
hReadEdge h = do
    line <- hGetLine h
    let from:to:[] = map read $ splitOn " " line
    return (from, to)

readGraph :: IO Graph
readGraph = hReadGraph stdin

readEdge :: IO Edge
readEdge = hReadEdge stdin
