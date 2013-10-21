module Week4V2 where

import SetOrd
import System.Random
import Week3FromLecture
import TechniquesWeek3FromLecture


-- Exercise 2
-- Implement a random data generator for the datatype Set Int, where Set is as defined in http://homepages.cwi.nl/~jve/rcrh/SetOrd.hs
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

genRandomSet :: IO (Set Int)
genRandomSet = list2set genIntList


























