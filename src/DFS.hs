{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | A few versions of depth-first search for inductive graphs. A much
--   more comprehensive implementation comes with fgl directly as
--   @Data.Graph.Inductive.Query.DFS@.
--
--   This code was written for my blog post on generating mazes:
--   <http://jelv.is/blog/Generating Mazes with Inductive Graphs/>
module DFS where

import           Control.Applicative  ((<$>))
import           Control.Monad        (liftM2, liftM3)
import           Control.Monad.Random (MonadRandom, getRandomR)
import           Control.Monad.ST     (ST, runST)
import           Data.Array

import           Data.Graph.Inductive (Gr, labEdges, match, matchAny, nodes,
                                       order, (&))
import qualified Data.Graph.Inductive as Graph
import           Data.List            (sortBy)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust)
import           Debug.Trace          (trace)
import           UnionFind

-- | Return an arbitrary node from the graph. Fails on empty graphs.
ghead :: Gr a b -> Graph.Node
ghead graph | Graph.isEmpty graph           = error "Empty graph!"
ghead (matchAny -> ((_, node, _, _), graph)) = node

-- | An unordered map that relabels all the nodes in the graph.
mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes _ g | Graph.isEmpty g = Graph.empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g

-- | The simplest depth-first search which returns a list of the nodes
--   visited, in order.
dfs :: Graph.Node -> Gr n e -> [Graph.Node]
dfs start graph = go [start] graph
  where go [] _                           = []
        go _ g | Graph.isEmpty g          = []
        go (n:ns) (match n -> (Just c, g)) =
          n : go (Graph.neighbors' c ++ ns) g
        go (_:ns) g                       = go ns g

-- | A modified version of dfs that returns a list of edges followed
--   rather than just nodes. This will error out if the start node is
--   not present in the graph (ie if the graph is empty).
edfs :: Graph.Node -> Gr n e -> [Graph.LEdge e]
edfs start (match start -> (Just ctx, graph)) =
  normalize $ go (lNeighbors' ctx) graph
  where go [] _                                = []
        go _ g | Graph.isEmpty g               = []
        go ((p, n, l):ns) (match n -> (Just c, g)) =
          (p, n, l) : go (lNeighbors' c ++ ns) g
        go (_:ns) g                            = go ns g

-- | Return the edges to the neighbors of a given context
lNeighbors' :: Graph.Context n e -> [Graph.LEdge e]
lNeighbors' c = [(p, n, l) | (n, l) <- Graph.lpre' c ++ Graph.lsuc' c]
  where p = Graph.node' c

-- | Reorders edges to be consistent. We need this because we're
--   treating our edges as undirected.
normalize :: [Graph.LEdge e] -> [Graph.LEdge e]
normalize = map swap
  where swap (n, n', l)
          | n < n'    = (n', n, l)
          | otherwise = (n, n', l)

-- | A version of edfs where the order neighbors are visited is
--   random.
edfsR :: MonadRandom m => Graph.Node -> Gr n e -> m [Graph.LEdge e]
edfsR start (match start -> (Just ctx, graph)) =
  normalize <$> go (lNeighbors' ctx) graph
  where go [] _                                = return []
        go _ g | Graph.isEmpty g               = return []
        go ((p, n, l):ns) (match n -> (Just c, g)) = do
          edges <- shuffle2 $ lNeighbors' c
          ((p, n, l) :) <$> go (edges ++ ns) g
        go (_:ns) g                            = go ns g

sequenceLEdge :: Functor f => Graph.LEdge (f a) -> f (Graph.LEdge a)
sequenceLEdge (l, r, act) = fmap (\v -> (l, r, v)) act


-- | MST-Kruskal alg.
kruskal :: (Ord e) => Map.Map Graph.Node Int  -> Int -> [Graph.LEdge e] -> [Graph.LEdge e]
kruskal symtab og l = runST $ uf >>= (\x -> kruskal' symtab x l [])
  where uf = newUnionFind og

kruskal' :: (Ord e) => Map.Map Graph.Node Int -> UnionFind s -> [Graph.LEdge e] -> [Graph.LEdge e] -> ST s [Graph.LEdge e]
kruskal' symtab uf [] res = return res
kruskal' symtab uf ((n1,n2,w):es) res = do
  found <- find uf i1 i2
  if not found then do
    UnionFind.unite uf i1 i2
    kruskal' symtab uf es ((n1,n2,w) : res)
    else
      kruskal' symtab uf es res
  where i1 = fromJust $ Map.lookup n1 symtab
        i2 = fromJust $ Map.lookup n2 symtab


-- | A naïve but unbiased list shuffle. Warning: it runs in O(n²)
--   time!
shuffle :: MonadRandom m => [a] -> m [a]
shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  (x :) <$> shuffle xs
  where choose [] = error "Cannot choose from empty list!"
        choose ls = do
          i <- getRandomR (0, length ls - 1)
          let (as, x:bs) = splitAt i ls
          return (x, as ++ bs)

-- | Durstenfeld's modification of the Fisher–Yates shuffle runs in O(n) time!
-- We convert ls into an Array arr first for fast indexing.
shuffle2 :: MonadRandom m => [t] -> m [t]
shuffle2 [] = return []
shuffle2 ls = elems <$> doSwaps (length ls) arr
  where arr = listArray (1, length ls) ls
        doSwaps 1 arr = return arr
        doSwaps len arr = do
          i <- getRandomR(1,len)
          doSwaps (len - 1) $ (arr //) [(len,arr!i),(i,arr!len)]
