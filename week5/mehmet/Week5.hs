module AW5

where

import Week2
import Week3
import Week4
import Week5

import AW2
import AW3
import AW4

import Data.List


forall = flip all
    
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x ==> q x)
weaker xs p q = stronger xs q p

test1 = stronger [1..10] (\ x -> even x && x > 3) even
test2 = stronger [1..10] (\ x -> even x || x > 3) even
test3 = stronger [1..10] (\ x -> (even x && x > 3) || even x) even
test4 = stronger [1..10] even (\ x -> (even x && x > 3) || even x)
        

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 1. The function merge from the course notes can be used as follows, to    │
-- │    create a function for list sorting:                                    │
-- └───────────────────────────────────────────────────────────────────────────┘
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)
-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Find a suitable assertion, and write an assertive version of this.        │
-- │ Deliverables: Assertion, Haskell program that uses this assertion,        │
-- │ indication of time spent. (3 hours) mostly studing assert1                │
-- └───────────────────────────────────────────────────────────────────────────┘  
mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 (\ a    b -> sorted b) mergeSrt
--                  (  a -> b -> Bool ) -> (a -> b) -> a -> b

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 2. Another approach to merge sort is to start by splitting the list to be │ 
-- │    sorted in equal parts, recursively sort the parts, next merge.         │ 
-- │    Implement this, using the following split function.                    │
-- └───────────────────────────────────────────────────────────────────────────┘
split :: [a] -> ([a],[a])
split xs = 
  let n = (length xs) `div` 2
  in (take n xs, drop n xs)
-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Find a suitable assertion, and write an assertive version of this.        │
-- │ Deliverables: Assertion, Haskell program that uses this assertion,        │
-- │ indication of time spent. (3 hours) mostly studing assert1                │
-- └───────────────────────────────────────────────────────────────────────────┘
splitA :: [a] -> ([a],[a])
splitA = assert1 (\ xs (ys,zs) -> True ) split  

mergeSrtWithSplit :: Ord a => [a] -> [a]
mergeSrtWithSplit []     = []
mergeSrtWithSplit [x]    = [x]
mergeSrtWithSplit (x:xs) = 
  let (ys,zs) = split (x:xs) 
  in merge (mergeSrtUsingSplit ys) (mergeSrtUsingSplit zs)

mergeSrtWithSplitA :: Ord a => [a] -> [a]
mergeSrtWithSplitA = assert1 (\ xs ys -> sorted ys) mergeSrtWithSplit

-- Test Arrays
test_Array1 = [0,12,1,2,4,5,1]
test_Array2 = [1,2,3,4,3,2,6,3,8,4,9]
test_Array3 = [0,1,1,2,4,5,12]            -- test_Array1 sorted
test_Array4 = [1,2,2,3,3,3,4,4,6,8,9]     -- test_Array2 sorted
test_Array5 = ([0,12,1],[2,4,5,1] )       -- test_Array1 Split
test_Array6 = ([1,2,3,4,3],[2,6,3,8,4,9]) -- test_Array2 Split

-- test mergeSrtA:
test1_mergeSrtA = (mergeSrtA test_Array1) == (mergeSrt test_Array1) -- True
test2_mergeSrtA = (mergeSrtA test_Array1) == test_Array3            -- True
test3_mergeSrtA = (mergeSrtA test_Array2) == test_Array3            -- False

-- test splitA
test4_splitA = (splitA test_Array1) == (split test_Array1) -- True
test5_splitA = (splitA test_Array1) == test_Array5         -- True
test6_splitA = (splitA test_Array2) == test_Array5         -- False


-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 3. The goal of this exercise is to extend the sudoku program from the     │
-- │    course notes with functions that can also handle sudokus of a special  │
-- │    kind: the sudokus that appear in NRC-Handelsblad each week (designed   │
-- │    by Peter Ritmeester, from Oct 8, 2005 onward). These NRC sudokus are   │
-- │    special in that they have to satisfy a few extra constraints: in       │
-- │    addition to the usual sudoku constraints, each of the 3 STX 3 subgrids │
-- │    with left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield a│
-- │    surjective function. Here is an example (the sudoku exercise of        │ 
-- │    Saturday Nov 26, 2005):                                                │
-- │                                                                           │
-- │    +---------+---------+---------+                                        │
-- │    |         | 3       |         |                                        │
-- │    |   +-----|--+   +--|-----+   |                                        │
-- │    |   |     | 7|   |  | 3   |   |                                        │
-- │    | 2 |     |  |   |  |     | 8 |                                        │
-- │    +---------+---------+---------+                                        │
-- │    |   |   6 |  |   |5 |     |   |                                        │
-- │    |   +-----|--+   +--|-----+   |                                        │
-- │    |    9  1 | 6       |         |                                        │
-- │    |   +-----|--+   +--|-----+   |                                        │
-- │    | 3 |     |  | 7 |1 | 2   |   |                                        │
-- │    +---------+---------+---------+                                        │
-- │    |   |     |  |   |  |    3| 1 |                                        │
-- │    |   |8    |  | 4 |  |     |   |                                        │
-- │    |   +-----|--+   +--|-----+   |                                        │
-- │    |       2 |         |         |                                        │
-- │    +---------+---------+---------+                                        │
-- │                                                                           │
-- │    Your task is to formalize this extra constraint, and to use your       │
-- │    formalization in a program that can solve this sudoku. Deliverables:   │
-- │    formal statement of new constraint, modified Haskell program, sudoku   │
-- └───────────────────────────────────────────────────────────────────────────┘
-- We made modification to 'lib/Week5FromLecture.hs' so that we can check 
-- consistency of subgrid (2,2) (2,6) (6,2) (6,6) and also get freePosAt 
-- subgrid [1,4,7] and subgrid [2,6]. The new function is ended by ' so for 
-- example bl' subGrid' etc.

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 4. The course notes of this week contain a sudoku solver. A sudoku        │
-- │    generator written in Haskell is available on the course web page, as   │
-- │    RandomSudoku.hs. Use your program from the previous exercise and this  │
-- │    program to create a program that generates NRC-Handelsblad sudoku      │
-- │    problems. Deliverables: NRC-Handelsblad sudoku generator, indication   │
-- │    of time spent. (2 Hours)                                               │
-- └───────────────────────────────────────────────────────────────────────────┘

gen_N_Random_NRC_Sudoku :: Int -> IO()
gen_N_Random_NRC_Sudoku n = do 
	if n <= 0 
	then error ("Input must be greater then 0\n Example gen_N_Random_NRC_Sudoku 4")
	else do
		if n == 1 
		then do
			print ("NRC Sudoku Problem:" ++ show n)
			s <- genRandomNodeProblem
			showNode s
			
		else do
			gen_N_Random_NRC_Sudoku (n-1)
			print ("NRC Sudoku Problem:" ++ show n)
			s <- genRandomNodeProblem
			showNode s
			
genRandomNodeProblem :: IO Node
genRandomNodeProblem = do 
	[r] <- rsolveNs [emptyN]
	s   <- genProblem r
	return s

	
showNode' :: Node -> Grid
showNode' = sud2grid . fst	

genRandomGrid :: IO Grid
genRandomGrid = do
	s <- genRandomNodeProblem
	return (showNode' s)
	
genRandomGrids :: Int -> IO [Grid]
genRandomGrids 0 = return []
genRandomGrids n = do 
	g  <- genRandomGrid
	gs <- genRandomGrids (n-1) 
	return (g:gs)	
