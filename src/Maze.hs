-- | Code for generating a maze from a randomized graph traversal. A
--   maze is just represented by a list of the walls to draw.
--
--   This code was written for my blog post on generating mazes:
--   <http://jelv.is/blog/Generating Mazes with Inductive Graphs/>
module Maze where

import           Control.Applicative  ((<$>))
import           Control.Monad        (forM_, liftM, mapM, sequence_)
import           Control.Monad.Random
import           DFS

import           Data.Graph.Inductive (Gr, edgeLabel, labEdges, nodes, order,
                                       prettyPrint)
import qualified Data.Graph.Inductive as Graph
import           Data.List            (sortBy, (\\))
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set, empty, member, singleton, union)
import           UnionFind

-- | Since we're starting with a grid, we only have two possible
--   orientations for walls.
data Orientation = Horizontal | Vertical deriving (Show, Eq)

-- | A wall is positioned relative to the cell that is *below* or *to
--   the right* of it.
data Wall = Wall (Int, Int) Orientation deriving (Eq)

instance Show Wall where
  show (Wall (x, y) Horizontal) = show (x, y) ++ " —"
  show (Wall (x, y) Vertical)   = show (x, y) ++ " |"

data WeightedWall = WeightedWall (Int, Int, Int) Orientation deriving (Eq)

instance Ord WeightedWall where
  compare (WeightedWall (_, _, a) _) (WeightedWall (_, _, b) _) = compare a b

instance Show WeightedWall where
  show (WeightedWall (x, y, w) Horizontal) = show (x, y, w) ++ " —"
  show (WeightedWall (x, y, w) Vertical)   = show (x, y, w) ++ " |"
-- -- | We start with a graph representing a graph with nodes as cells
-- --   and edges as walls.
type Grid = Gr () Wall

-- | Generate a graph representing an n × m grid.
grid :: Int -> Int -> Grid
grid width height = Graph.mkGraph nodes edges
  where nodes = [(node, ()) | node <- [0..width * height - 1]]
        edges = [(n, n', wall n Vertical) |
                 (n, _) <- nodes,
                 (n', _) <- nodes,
                 n - n' == 1 && n `mod` width /= 0 ]
             ++ [(n, n', wall n Horizontal) |
                 (n,_) <- nodes,
                 (n',_) <- nodes,
                 n - n' == width ]
        wall n = let (y, x) = n `divMod` width in Wall (x, y)

weightedGrid :: MonadRandom m => Int -> Int -> Gr () (m WeightedWall)
weightedGrid width height = Graph.mkGraph nodes edges
  where nodes = [(node, ()) | node <- [0..width * height - 1]]
        edges = [(n, n', ww n width Vertical) |
                 (n, _) <- nodes,
                 (n', _) <- nodes,
                 n - n' == 1 && n `mod` width /= 0 ]
             ++ [(n, n', ww n width Horizontal) |
                 (n,_) <- nodes,
                 (n',_) <- nodes,
                 n - n' == width ]
        ww n width orientation = do
         w <- getRandomR(1, 100) -- random constant
         let (y, x) = n `divMod` width in
           return (WeightedWall (x, y, w) orientation)


-- | Generates the random edge traversal of an n × m grid.
generate :: MonadRandom m => Int -> Int -> m [Graph.LEdge Wall]
generate width height = do
  x <- (Graph.labEdges graph \\) <$> edfsR (ghead graph) graph
  return x
  where graph = grid width height

-- | Generates the MST edge traversal of an n × m grid.
generateMST :: MonadRandom m => Int -> Int -> m [Graph.LEdge WeightedWall]
generateMST width height = do
  x <- mapM sequenceLEdge (Graph.labEdges graph)
  let y = kruskal mapl (order graph) (sortBy sf x)
  return (x \\ y)
    where graph = weightedGrid width height
          sf (_,_,c) (_,_,f) = compare c f
          mapl = Map.fromList $ zip (nodes graph) [0..(order graph -1)]

-- | Look up an edge label in the given graph. I don't know why this
--   isn't in the standard fgl API!
edgeLabel :: Gr a b -> Graph.Edge -> b
edgeLabel graph (n, n') = label
  where edges = Graph.labEdges graph
        Just (_, _, label) = List.find (\ (a, b, l) -> a == n && b == n') edges

-- | Generates a random n × m maze.
maze :: MonadRandom m => Int -> Int -> m [Wall]
maze width height = map Data.Graph.Inductive.edgeLabel <$> generate width height

-- | Generates a random n × m maze using a MST algorithm.
mazeMST :: MonadRandom m => Int -> Int -> m [WeightedWall]
mazeMST width height = map Data.Graph.Inductive.edgeLabel <$> generateMST width height

pp :: WeightedWall -> String
pp (WeightedWall (a, b, c) _) = show a ++ " " ++ show b ++ " " ++ show c

test :: IO ()
test = do
  ww <- mapM Data.Graph.Inductive.edgeLabel $ labEdges (weightedGrid 10 10)
  forM_ (map pp ww) putStrLn
