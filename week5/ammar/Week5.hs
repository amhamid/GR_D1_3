module Week5

where

import Data.List
import Week5FromLecture
import RandomSudoku


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

-- For demonstration purpose, we created example6 grid for this exercise:

--
--Week5> showGrid example6

-- +---------+---------+---------+
-- |         | 3       |         |
-- |   +-----|--+   +--|-----+   |
-- |   |     | 7|   |  | 3   |   |
-- | 2 |     |  |   |  |     | 8 |
-- +---------+---------+---------+
-- |   |   6 |  |   |5 |     |   |
-- |   +-----|--+   +--|-----+   |
-- |    9  1 | 6       |         |
-- |   +-----|--+   +--|-----+   |
-- | 3 |     |  | 7 |1 | 2   |   |
-- +---------+---------+---------+
-- |   |     |  |   |  |    3| 1 |
-- |   |8    |  | 4 |  |     |   |
-- |   +-----|--+   +--|-----+   |
-- |       2 |         |         |
-- +---------+---------+---------+


-- *Week5> solveAndShow example6

-- +---------+---------+---------+
-- | 4  7  8 | 3  9  2 | 6  1  5 |
-- |   +-----|--+   +--|-----+   |
-- | 6 |1  9 | 7| 5 |8 | 3  2| 4 |
-- | 2 |3  5 | 4| 1 |6 | 9  7| 8 |
-- +---------+---------+---------+
-- | 7 |2  6 | 8| 3 |5 | 1  4| 9 |
-- |   +-----|--+   +--|-----+   |
-- | 8  9  1 | 6  2  4 | 7  5  3 |
-- |   +-----|--+   +--|-----+   |
-- | 3 |5  4 | 9| 7 |1 | 2  8| 6 |
-- +---------+---------+---------+
-- | 5 |6  7 | 2| 8 |9 | 4  3| 1 |
-- | 9 |8  3 | 1| 4 |7 | 5  6| 2 |
-- |   +-----|--+   +--|-----+   |
-- | 1  4  2 | 5  6  3 | 8  9  7 |
-- +---------+---------+---------+



-------------------------
-- exercise 4
-------------------------

-- in lib/RandomSudoku.hs there is already a function that generate the problem (genProblem)
-- This function already use our modified functions from lib/Week5FromLecture.hs that solve
-- NRC-Handelsblad Sudoku problem

-- show a random generated problem
showGeneratedProblem = do
			[r] <- rsolveNs [emptyN]
			showNode r
			s  <- genProblem r
			showNode s



-------------------------
-- exercise 5
-------------------------

-- minimal means: the mimimal amount of hints with a unique solution
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

testMinimal = do	
		g <- genRandomGrid
		return (and (map (uniqueSol) (initNode g)))


