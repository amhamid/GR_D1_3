module Week5

where

import Data.List
import Week5FromLecture


-------------------------
-- exercise 1
-------------------------

mergeSrt ::  Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] => [a]
mergeSrtA = assert1 (\ _ x -> sorted x) mergeSrt



-------------------------
-- exercise 2
-------------------------

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
           in (take n xs, drop n xs)


mergeSrtUsingSplit :: Ord a => [a] -> [a]
mergeSrtUsingSplit []= []
mergeSrtUsingSplit [x]= [x]
mergeSrtUsingSplit zs  = let (xs,ys) = split zs 
			 in merge (mergeSrtUsingSplit xs)  (mergeSrtUsingSplit ys)

mergeSrtUsingSplitA :: Ord a => [a] -> [a]
mergeSrtUsingSplitA = assert1 (\ _ x -> sorted x) mergeSrtUsingSplit



-------------------------
-- exercise 3
-------------------------

-- We made modification to 'lib/Week5FromLecture.hs' so that we can check consistency of subgrid (2,2) (2,6) (6,2) (6,6) and also get freePosAt subgrid [1,4,7] and subgrid [2,6].
-- The new function is ended by ' so for example bl' subGrid' etc.



-------------------------
-- exercise 4
-------------------------





