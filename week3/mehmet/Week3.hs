module AW3

where

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║ Imports                                                                   ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝
import AW2
import Week3
import Techniques
import Data.List


-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 3. Consult the course slides of this week to write a generator for random │
-- │    integer lists.                                                         │
-- └───────────────────────────────────────────────────────────────────────────┘
genIntList :: IO [Int]
genIntList = do n <- getRandomInt 10
                d <- getRandomInt 10
                getRandomItems d n

getRandomItems :: Int -> Int -> IO [Int]
getRandomItems _ 0 = return []
getRandomItems d n = do 
                     f  <- getRandomInt d
                     fs <- getRandomItems d (n-1) 
                     return (f:fs)

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 4. A permutation of a finite list is another finite list with the same    │
-- │    elements, but possibly in a different order. For example, [0,2,0] is a │
-- │    permutation of [0,0,2], but [2,2,0] is not. Write a function           │
-- └───────────────────────────────────────────────────────────────────────────┘
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []    []     = False
isPermutation [x]   []     = False
isPermutation []    [y]    = False
isPermutation [xs]  [ys]    = any ([x]==) (permutations [y])
isPermutation (x:xs) (y:ys) = any ((x:xs)==) (permutations (y:ys))

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 5. Define some testable properties for this function, and use your random │
-- │    generator for integer lists from Exercise 3 to test isPermutation.     │
-- └───────────────────────────────────────────────────────────────────────────┘
