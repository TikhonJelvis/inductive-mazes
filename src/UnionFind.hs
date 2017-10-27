module UnionFind where
import           Control.Monad    (liftM2)
import           Control.Monad.ST (ST)
import           Data.Array.ST    (STUArray, newArray, newListArray, readArray,
                                   writeArray)

-- From http://kseo.github.io/posts/2014-01-30-implementing-union-find-in-haskell.html
data UnionFind s = UnionFind {
    ids:: STUArray s Int Int
  , szs:: STUArray s Int Int
  }

newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)

find :: UnionFind s -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: UnionFind s -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if id /= i
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i

unite :: UnionFind s -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if szi < szj
        then do
            writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
        else do
            writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)
