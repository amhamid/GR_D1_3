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





