module Week3 where

import Week3FromLecture
import TechniquesWeek3FromLecture
import System.Random
import Data.List


-- exercise 3
genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d n = do
		f <- getRandomInt d
		fs <- genIntList' d (n-1)
		return (f:fs)

genIntList :: IO [Int]
genIntList = genIntList' 100 20



-- exercise 4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
		    | xs == ys = True
		    | otherwise = or (map (==xs) (permutations ys))



-- exercise 5
--
-- Testable properties: 
-- 1. if the length of array1 and array2 are different
-- 2. if the length of array1 and array2 are the same then check if array1 is a permutation of array2

testIsPermutation' :: Int -> IO ()
testIsPermutation' 0 = do print("=== test finished ===")
testIsPermutation' n = do
			xs <- genIntList' 3 4
			ys <- genIntList' 3 4
			if isPermutation xs ys
			then do print ("PASS on: " ++ show xs ++ " ::: " ++ show ys ++ "   *")
			else do print ("fail on: " ++ show xs ++ " ::: " ++ show ys)
			testIsPermutation' (n-1)


testIsPermutation :: IO ()
testIsPermutation = testIsPermutation' 50



-- exercise 6




