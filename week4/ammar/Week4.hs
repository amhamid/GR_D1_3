module Week4 where

import SetOrd
import System.Random
import TechniquesWeek3FromLecture
import Data.List


-- exercise 2
--
-- precondition is d >= n
-- d is the range of random number
-- n is how many elements in the set
--
genRandomSetInt' :: Int -> Int -> Set Int -> IO (Set Int)
genRandomSetInt' d n (Set s) | d < n = return (error "the first argument must be bigger or equal than the second argument")
                             | length s /= n =  do x <- getRandomInt d
                                                   Set xs <- genRandomSetInt' d n (insertSet x (Set s))
                                                   return (Set xs)
                             | otherwise = return (Set s)

genRandomSetInt :: Int -> Int -> IO (Set Int)
genRandomSetInt d n = genRandomSetInt' d n emptySet



-- exercise 3
-- unionSet is already defined in lib/SetOrd.hs
--
intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) _ = Set []
intersectionSet (Set (x:xs)) (Set ys) | (inSet x (Set (sort ys))) = insertSet x (intersectionSet (Set xs) (Set ys))
				      | otherwise = intersectionSet (Set xs) (Set ys)



differenceSet' :: (Ord a) => Set a -> Set a -> Set a
differenceSet' (Set []) _ = Set []
differenceSet' (Set (x:xs)) set2 | (not (inSet x set2)) = insertSet x (differenceSet' (Set xs) set2)
                                 | otherwise = differenceSet' (Set xs) set2
  
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = unionSet (differenceSet' (Set xs) (Set ys)) (differenceSet' (Set ys) (Set xs))
                                

-- Testing properties (for both intersectionSet and differenceSet)
-- 1. length xs > length ys
-- 2. length xs == length ys
-- 3. length xs < length ys
-- 4. no common element (different random range)
-- 5. all the same element
--
--
-- TODO create test methods.
--
--
--
--



-- exercise 4

  
  
