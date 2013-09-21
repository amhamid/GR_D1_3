module Week3 where

import Week3FromLecture
import TechniquesWeek3FromLecture
import System.Random
import Data.List
import Week2 -- in lib folder
import Week2FromLecture hiding (Neg)
import Data.Char


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
testCnf :: IO ()
testCnf = testForms 10 (\ f -> let g = (cnf(nnf(arrowfree f))) in equiv f g)



-- exercise 7
getRandomInts :: Int -> Int -> IO Int
getRandomInts n m = getStdRandom (randomR (n,m))

getRandomFormula :: Int -> IO Formula
getRandomFormula 0 = do m <- getRandomInts 97 122  -- 97 is char 'a' and 122 is char 'z'
                        return (Atom ([chr m]) [])

getRandomFormula d = do n <- getRandomInt 5
			case n of
				0 -> do m <- getRandomInts 97 122
					return (Atom ([chr m]) [])
				1 -> do f <- getRandomFormula (d-1)
					return (Neg f)
				2 -> do m  <- getRandomInt 5
					fs <- getRandomFormulas (d-1) m
					return (Conj fs)
				3 -> do m  <- getRandomInt 5
					fs <- getRandomFormulas (d-1) m
					return (Disj fs)
				4 -> do m <- getRandomInts 97 122
					f <- getRandomFormula (d-1)
					return (Forall ([chr m]) f)
				5 -> do m <- getRandomInts 97 122
					f <- getRandomFormula (d-1)
					return (Exists ([chr m]) f)

getRandomFormulas :: Int -> Int -> IO [Formula]
getRandomFormulas _ 0 = return []
getRandomFormulas d n = do
			f <- getRandomFormula d
			fs <- getRandomFormulas d (n-1)
			return (f:fs)



