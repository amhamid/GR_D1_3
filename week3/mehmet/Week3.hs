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
-- precondition: input X is sorted [a]
-- precondition: input Y is sorted [a]
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []     []     = False
isPermutation [xs]   []     = False
isPermutation []     [ys]   = False

isPermutation [x]    [y]    = 
  if x == y
  then True 
  else False
                             
isPermutation (x:xs) (y:ys) = 
  if x == y
  then isPermutation xs ys
  else False

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║ hulp functies                                                             ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Split:                                                                    │
-- │ web: http://en.literateprograms.org/Merge_sort_(Haskell)#chunk use:merge  │
-- └───────────────────────────────────────────────────────────────────────────┘
split :: [a] -> ([a],[a])   
split (x:y:zs) = (x:xs,y:ys) where (xs,ys) = split zs
split xs       = (xs,[])

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Merge:                                                                    │
-- │ web: http://en.literateprograms.org/Merge_sort_(Haskell)#chunk use:merge  │
-- └───────────────────────────────────────────────────────────────────────────┘
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge pred xs []         = xs
merge pred [] ys         = ys
merge pred (x:xs) (y:ys) =
  case pred x y of
    True  -> x: merge pred xs (y:ys)
    False -> y: merge pred (x:xs) ys

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Merge Sort:                                                               │
-- │ web: http://en.literateprograms.org/Merge_sort_(Haskell)#chunk use:merge  │
-- │ Example: mergesort (<=) [1, 5, 6, 4, 3]  Result : [1,3,4,5,6]             │
-- └───────────────────────────────────────────────────────────────────────────┘
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort pred []   = []
mergesort pred [x]  = [x]
mergesort pred xs = merge pred (mergesort pred xs1) (mergesort pred xs2)
 where
   (xs1,xs2) = split xs
