{-# LANGUAGE TupleSections #-}
module Graph (
  Vertex, Edge, Path, Graph(Graph, vertices, edges), verticesDegreeMap,
  inVertices, outVertices,
  adjacencyMatrix, incedenceMatrix, distMatrix, accessMatrix, outDeg, inDeg, deg,
  isHomogeneous, homogeneousDegree, leafVerticies, isolatedVerticies, simpleLoops,
  isStronglyConnected, isOnedirectionConnected, isWeaklyConnected, isNotConnected
) where

import Data.Array.IArray
import Data.List
import qualified Data.Map as M
import Data.Tuple

type Vertex = Int
type Edge = (Vertex, Vertex)
type Path = [Vertex]
data Graph = Graph {
    vertices :: [Vertex],
    edges :: [Edge]
} deriving (Show)

outVerticesMap :: Graph -> M.Map Vertex [Vertex]
outVerticesMap (Graph vertices edges) = foldr (\(v,u) m -> M.insertWith (++) v [u] m) beginMap edges
    where beginMap = M.fromList $ map (,[]) vertices

inVerticesMap :: Graph -> M.Map Vertex [Vertex]
inVerticesMap (Graph vertices edges) = foldr (\(v,u) m -> M.insertWith (++) u [v] m) beginMap edges
    where beginMap = M.fromList $ map (,[]) vertices

outVertices :: Graph -> Vertex -> [Vertex]
outVertices g v = outVerticesMap g M.! v
--outVertices (Graph vertices edges) v = [u | u <- vertices, (v,u) `elem` edges]


inVertices :: Graph -> Vertex -> [Vertex]
inVertices g v = inVerticesMap g M.! v
--inVertices (Graph vertices edges) v = [u | u <- vertices, (u,v) `elem` edges]

abcAdjacencyMatrix :: (a,a,a) -> Graph -> Array (Vertex, Vertex) a
abcAdjacencyMatrix (a,b,c) (Graph vertices edges) = array r [((i,j), is i j) | i <- vertices, j <- vertices]
    where is i j | (i,j) `elem` edges = a
                 | i == j             = c
                 | otherwise          = b
          r = ((1,1), (length vertices, length vertices))

adjacencyMatrix :: (Num a) => Graph -> Array (Vertex, Vertex) a
adjacencyMatrix = abcAdjacencyMatrix (1,0,0)

incedenceMatrix :: (Num a) => Graph -> Array (Vertex, Int) a
incedenceMatrix (Graph vertices edges) = array arrayRange
    (zip (range arrayRange) $ concat $ map (\vertex -> zipWith f (repeat vertex) edges) vertices)
    where
        arrayRange = ((1,1), (length vertices, length edges))
        f e (u,v) | u == v && e == u =  2
                  | e == u           =  1
                  | e == v           = -1
                  | otherwise        =  0


distMatrix :: (Num a, Eq a) => Graph -> Array (Vertex, Vertex) (Maybe a)
distMatrix g@(Graph vertices edges) = zipArrayWith f rdist d
    where
        rdist = distMatrix' g
        d = diagMatrix (Just 0, Nothing) (bounds rdist)
        f a b = if b /= Nothing then b
                                else a

distMatrix' :: (Num a, Eq a) => Graph -> Array (Vertex, Vertex) (Maybe a)
distMatrix' g@(Graph vertices edges) = array r ar
    where
        r = ((1,1), (length vertices, length vertices))
        ar = zip (range r) (concat $ map (M.elems . shortestWays g) vertices)

accessMatrix :: Graph -> Array (Vertex, Int) Int
accessMatrix = amap (\i -> case i of Nothing   -> 0
                                     otherwise -> 1) . distMatrix'

accessMatrix' :: Graph -> Array (Vertex, Int) Int
accessMatrix' = amap (\i -> case i of Nothing   -> 0
                                      otherwise -> 1) . distMatrix

shortestWays :: (Num a, Eq a) => Graph -> Vertex -> M.Map Vertex (Maybe a)
shortestWays g@(Graph vertices edges) v = shortFold startMap 0 [v]
    where
        ovm = outVerticesMap g
        startMap = M.fromList $ zip vertices (repeat Nothing)
        shortFold :: (Num a, Eq a) => M.Map Vertex (Maybe a) -> a -> [Vertex] -> M.Map Vertex (Maybe a)
        shortFold m _ [] = m
        shortFold m step verts = shortFold newM (step+1) newVerts
            where
                (newM, newVerts) = foldr sfold (m,[]) verts
                sfold v old@(m,verts) = if m M.! v == Nothing then (if step /= 0 then M.insert v (Just step) m
                                                                                 else m, verts ++ ovm M.! v)
                                                              else old

verticesDegreeMap :: Num a => Graph -> M.Map Vertex a
verticesDegreeMap (Graph vertices edges) = foldr foldf (M.fromList $ zip vertices (repeat 0)) edges
    where foldf (u,v) = ins u . ins v
          ins u = M.insertWith (+) u 1

outDeg :: Num a => Graph -> Vertex -> a
outDeg g = genericLength . outVertices g

inDeg :: Num a => Graph -> Vertex -> a
inDeg g = genericLength . inVertices g

deg :: Num a => Graph -> Vertex -> a
deg g v = inDeg g v + outDeg g v

isHomogeneous :: Graph -> Bool
isHomogeneous g = if homogeneousDegree g == Nothing then False
                                                    else True

homogeneousDegree :: (Num a, Eq a) => Graph -> Maybe a
homogeneousDegree g = if isHomo then Just fst
                                else Nothing
    where
        isHomo = M.fold (\deg ac -> ac && deg == fst) True m
        fst = head (M.elems m)
        m = verticesDegreeMap g

verticesWithDegree :: (Eq a, Num a) => a -> Graph -> [Vertex]
verticesWithDegree n g = filter ((==n) . deg g) . vertices $ g

leafVerticies :: Graph -> [Vertex]
leafVerticies = verticesWithDegree 1

isolatedVerticies :: Graph -> [Vertex]
isolatedVerticies = verticesWithDegree 0

simpleLoops :: Graph -> [Path]
simpleLoops g = filterUniqueCycles $ concatMap (simpleLoopsFrom m) (map (:[]) $ vertices g)
    where
      m = outVerticesMap g
      filterUniqueCycles :: [Path] -> [Path]
      filterUniqueCycles = (flip foldr []) $ \p ac ->
          let cycles = map (concat . replicate 2 . tail) ac :: [Path]
          in if any (p `isInfixOf`) cycles then ac
                                           else p:ac

simpleLoopsFrom :: M.Map Vertex [Vertex] -> Path -> [Path]
simpleLoopsFrom m p
    | isSimpleLoop p = [p]
    | otherwise = concatMap (simpleLoopsFrom m) (map (:p) ov)
        where
            first = head p
            ov = filter (not . (`elem` (init p))) $ m M.! first
            isSimpleLoop p = length p > 1 && head p == last p

isStronglyConnected :: Graph -> Bool
isStronglyConnected = all (==1) . elems . accessMatrix'

isOnedirectionConnected :: Graph -> Bool
isOnedirectionConnected g@(Graph vertices _) = and [am!(i,j) == 1 || am!(j,i) == 1 | i <- vertices, j <- vertices]
    where am = accessMatrix' g

isWeaklyConnected :: Graph -> Bool
isWeaklyConnected = isStronglyConnected . toUndirected

isNotConnected :: Graph -> Bool
isNotConnected = not . isWeaklyConnected

toUndirected :: Graph -> Graph
toUndirected (Graph vertices edges) = Graph vertices $ concatMap (\x -> [x, swap x]) edges

----------------------------------- Utils --------------------------------------------------------

diagMatrix :: (Num a, Ix a) => (b,b) -> ((a,a),(a,a)) -> Array (a,a) b
diagMatrix (a,b) r = array r $ map f (range r)
    where
        f x@(i,j) = if i == j then (x,a)
                              else (x,b)

zipArrayWith :: (Ix i) => (a -> a -> b) -> Array i a -> Array i a -> Array i b
zipArrayWith f a b
    | bounds a /= bounds b = error "array bounds diffs"
    | otherwise = listArray (bounds a) $ zipWith f (elems a) (elems b)
