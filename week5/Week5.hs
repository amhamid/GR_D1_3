module Week5

where

import Data.List
import Week5FromLecture
import RandomSudoku
import Data.Time.Clock


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



-- +---------------------------------------------------------------------------+
-- ¦ 4. The course notes of this week contain a sudoku solver. A sudoku        ¦
-- ¦    generator written in Haskell is available on the course web page, as   ¦
-- ¦    RandomSudoku.hs. Use your program from the previous exercise and this  ¦
-- ¦    program to create a program that generates NRC-Handelsblad sudoku      ¦
-- ¦    problems. Deliverables: NRC-Handelsblad sudoku generator, indication   ¦
-- ¦    of time spent. (2 Hours)                                               ¦
-- +---------------------------------------------------------------------------+

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

-- +---------------------------------------------------------------------------+
-- ¦ 5. Test your programs from the previous two exercises, and document the   ¦
-- ¦    test process. One important property to test is whether the generated  ¦
-- ¦    sudoku problems are minimal. How can you test this?                    ¦
-- ¦    Deliverables: testing code, test report, indication of time spent.     ¦
-- +---------------------------------------------------------------------------+


-- minimal means: the mimimal amount of hints with a unique solution

testMinimal = do
	g <- genRandomGrid
	return (and (map (uniqueSol) (initNode g)))

-- here are some demonstration to find out the minimal number of hints with a unique solution.
-- this can bu tested by counting the non zero's
tst_NRCS = 	[[[0,0,0,0,0,0,0,8,1]
            	,[0,0,3,2,0,0,0,0,0]
        	,[0,9,0,8,0,0,2,0,0]
        	,[0,0,0,0,0,0,0,0,0]
		,[0,5,0,0,0,0,6,0,0]
		,[0,0,0,0,0,0,0,0,0]
		,[1,0,0,0,0,0,0,0,0]
		,[0,6,0,0,0,0,5,0,0]
		,[0,0,7,0,6,0,0,4,3]],
		[[0,0,0,0,0,0,0,5,0]
		,[2,3,0,0,1,0,0,0,0]
		,[7,0,0,0,0,0,0,0,9]
		,[0,0,0,0,0,0,0,7,0]
		,[4,0,5,0,0,0,0,1,0]
		,[0,9,0,0,5,2,0,0,0]
		,[0,0,0,0,0,0,0,0,0]
		,[1,0,0,2,0,0,0,0,0]
		,[0,0,0,8,0,9,0,0,0]]]

-- Minimal Grid, counts the non Zeros
minimalGrid :: (Num a, Ord a) => [[a]] -> Int
minimalGrid [[]]   = 0
minimalGrid [x]    = length (filter (>0) x)
minimalGrid (x:xs) = (length (filter (>0) x)) + (minimalGrid xs)

-- test a n-number of random Grid and put the minimal in a array and find the lowest minimal
find_Minimals_Of_NRC :: Int -> IO [Int]
find_Minimals_Of_NRC 0 = return []
find_Minimals_Of_NRC n = do 
	g <- genRandomGrid
	let m = minimalGrid g
	ms <- find_Minimals_Of_NRC (n-1) 
	return (m:ms)
	
-- TODO
test = do
	g <- genRandomGrid
	return (and (map (uniqueSol) (initNode g)))

find_Minimal_Of_NRC :: IO()
find_Minimal_Of_NRC = do
	
	-- Start Time
	st <- time
	print ("Start   : " ++ show  st)
	
	-- find the minimals of random NRC's
	m <- find_Minimals_Of_NRC 4
	print ("Minimals: " ++ show m)
	
    -- End Time
	et <- time
	print ("End     : " ++ show et)
		
	let dt = et - st
	print ("Diff    : " ++ show dt)

{-
result:

"Start   :49744.7264903s"
"Minimals:[16,17,16,19]"
"End     :49944.7119127s"
"Diff    :199.9854224s"	
-}

-- get time in seconds		
time = getCurrentTime >>= return . utctDayTime	


