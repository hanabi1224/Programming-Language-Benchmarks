--
-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Don Stewart
-- *reset*
--

import System.Environment
import Data.Bits
import Text.Printf

--
-- an artificially strict tree. 
--
-- normally you would ensure the branches are lazy, but this benchmark
-- requires strict allocation.
--
data Tree = Nil | Node !Int !Tree !Tree

minN = 4

io s n t = printf "%s of depth %d\t check: %d\n" s n t

main = do
    n <- getArgs >>= readIO . head
    let maxN     = max (minN + 2) n
        stretchN = maxN + 1

    -- stretch memory tree
    let c = check (make 0 stretchN)
    io "stretch tree" stretchN c

    -- allocate a long lived tree
    let !long    = make 0 maxN

    -- allocate, walk, and deallocate many bottom-up binary trees
    let vs = depth minN maxN
    mapM_ (\((m,d,i)) -> io (show m ++ "\t trees") d i) vs

    -- confirm the the long-lived binary tree still exists
    io "long lived tree" maxN (check long)

-- generate many trees
depth :: Int -> Int -> [(Int,Int,Int)]
depth d m
    | d <= m    = (n,d,sumT d n 0) : depth (d+2) m
    | otherwise = []
  where n = 1 `shiftL` (m - d + minN)

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT d 0 t = t
sumT  d i t = sumT d (i-1) (t + a)
  where a = check (make 0 d)


-- traverse the tree, counting up the nodes
check :: Tree -> Int
check Nil          = 0
check (Node i l r) = 1 + check l + check r

-- build a tree
make :: Int -> Int -> Tree
make i 0 = Node i Nil Nil
make i d = Node i (make d d2) (make d2 d2)
  where d2 = d-1