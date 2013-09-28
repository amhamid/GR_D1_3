module Week4 where

import SetOrd
import System.Random
import TechniquesWeek3FromLecture
import Data.List

------------------
-- exercise 2
------------------
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



------------------
-- exercise 3
------------------
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
-- TODO create test methods.
--
--

------------------
-- exercise 4
------------------

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Eq a => Rel a -> Rel a
trClos [] = []
trClos (x:xs) = ([x] ++ ([x] @@ (trClos xs))) ++ (trClos xs)  



------------------
-- exercise 5
------------------
--
-- Testing properties:
-- 
-- 1. if all elements are not connected then the length should be the same with the input
 
-- 2. if some of the elements are connected then the length of transitive closure should be bigger than the length of the input

-- 3. if all elements are all connected then the length is the sum of n + (n-1) + ... + 1 and 
--    [(a,b)....(x,y)] then the element (a,y) should be in the result


-- test properties 1
testAllConnectedTrClos' :: Eq a => Rel a -> Bool
testAllConnectedTrClos' x = length(trClos x) == (foldr (+) 0 [1..(length x)])
testAllConnectedTrClos = testAllConnectedTrClos' [ (x,y) | x <- [0..5], y <- [0..5],  x == y-1 ]


-- test properties 2
testSomeConnectedTrClos' :: Eq a => Rel a -> Bool
testSomeConnectedTrClos' x = length(trClos x) > (length x) 
testSomeConnectedTrClos = testSomeConnectedTrClos' [ (x,y) | x <- [0..3], y <- [2..5] ]


-- test properties 2
testAllNotConnectedTrClos' :: Eq a => Rel a -> Bool
testAllNotConnectedTrClos' x = length(trClos x) == (length x) 
testAllNotConnectedTrClos = testAllNotConnectedTrClos' [ (x,y) | x <- [0..3], y <- [4..6] ]


