{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | A few versions of depth-first search for inductive graphs. A much
--   more comprehensive implementation comes with fgl directly as
--   @Data.Graph.Inductive.Query.DFS@.
-- 
--   This code was written for my blog post on generating mazes:
--   <http://jelv.is/blog/Generating Mazes with Inductive Graphs/>
module DFS where

import           Control.Monad        (liftM)
import           Control.Monad.Random (getRandomR, MonadRandom)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, (&), match, matchAny)

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
  liftM normalize $ go (lNeighbors' ctx) graph
  where go [] _                                = return []
        go _ g | Graph.isEmpty g               = return []
        go ((p, n, l):ns) (match n -> (Just c, g)) = do
          edges <- shuffle $ lNeighbors' c
          liftM ((p, n, l) :) $ go (edges ++ ns) g
        go (_:ns) g                            = go ns g

-- | A naïve but unbiased list shuffle. Warning: it runs in O(n²)
--   time!
shuffle :: MonadRandom m => [a] -> m [a]
shuffle [] = return []
shuffle ls = do
  (x, xs) <- choose ls
  liftM (x :) $ shuffle xs
  where choose [] = error "Cannot choose from emtpy list!"
        choose ls = do
          i <- getRandomR (0, length ls - 1)
          let (as, x:bs) = splitAt i ls
          return (x, as ++ bs)
