module Week3V2 where

import Week3FromLecture
import TechniquesWeek3FromLecture
import System.Random
import Data.List
import System.IO


-- Exercise 3
-- Consult the course slides of this week to write a generator for random integer lists. The type should be
genIntList :: IO [Int]
genIntList = do
				a <- getRandomInt 10
				b <- getRandomInt 10
				getRandomInts a b
				
getRandomInts :: Int-> Int -> IO [Int]
getRandomInts _ 0 = return []
getRandomInts a b = do
					f <- getRandomInt a
					fs <- getRandomInts a (b-1)
					return (f:fs)

-- Exercise 4
-- A permutation of a nite list is another nite list with the same elements, but possibly in a dierent order. For example, [0,2,0] is a permutation of [0,0,2], but [2,2,0] is not. Write a function
-- That returns True if its arguments are permutations of each other.
isPermutation :: Eq a => [a] -> [a] -> Bool































