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


mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = invar1 sorted mergeSrt



-------------------------
-- exercise 2
-------------------------

split :: [a] -> ([a],[a])
split xs = let
             n = (length xs) `div` 2
           in
             (take n xs, drop n xs)


mergeSrtUsingSplit :: Ord a => [a] -> [a]
mergeSrtUsingSplit zs = let
			   (xs, ys) = split zs
			in
			   merge (mergeSrt xs) (mergeSrt ys)

mergeSrtUsingSplitA :: Ord a => [a] -> [a]
mergeSrtUsingSplitA = invar1 sorted mergeSrtUsingSplit



-------------------------
-- exercise 3
-------------------------





