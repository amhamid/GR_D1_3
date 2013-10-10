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
-- Mehmet: this one is better then mine but results the same
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
		    | xs == ys = True
		    | otherwise = or (map (==xs) (permutations ys))
-- VVZ: you could also use filter and check if length is 1
-- VVZ: also, identical arrays is not a "property", it is a simple case (implication goes to the other direction)


-- exercise 5
--
-- Testable properties: 
-- 1. if the length of array1 and array2 are different
-- 2. if the length of array1 and array2 are the same then check if array1 is a permutation of array2
-- Mehmet: came up with the same testable property, made the results of the printing the test
--         result more extensive


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
-- Mehmet: test results needed to be written to file
-- max number of tests
test_max = 30

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ setVarWithString: produces a string and adds and 'newline' character      │
-- └───────────────────────────────────────────────────────────────────────────┘
setVarWithString :: String -> IO String
setVarWithString s = return (s ++ "\n")

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ setVarWithString: produces a string of '='                                │
-- └───────────────────────────────────────────────────────────────────────────┘
strDividerLine :: IO String
strDividerLine = return ("===========================================================================\n")

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ strResultFile: constant with the name of the file for the test results    │
-- └───────────────────────────────────────────────────────────────────────────┘
strResultFile = "Result.txt"

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ writeCNF_testResult: Prints results to file of varible 'strResultFile'    │
-- │ Inputs │ Type │ Description                                               │
-- │ t      │ Int  │ Number of the test                                        │
-- │ f      │ Form │ Formula of any sort                                       │
-- │ g      │ Form │ Formula of the CNF sort                                   │
-- │ r      │ Bool │ Result of the test equiv f g                              │
-- └───────────────────────────────────────────────────────────────────────────┘
writeCNF_testResult :: Int -> Form -> Form -> Bool -> IO ()
writeCNF_testResult t f g r = do

  -- write divider line
  line <- strDividerLine
  appendFile strResultFile line 
  
  -- write test number
  line <- (setVarWithString ("Test   : " ++ show t))
  appendFile strResultFile line 
  
  -- generate formula en append to file
  line <- (setVarWithString ("Formula: " ++ show f))
  appendFile strResultFile line 
  
  -- conver formula too CNF
  line <- (setVarWithString ("CNF    : " ++ show g))
  appendFile strResultFile line 
  
  -- test if f and g and equivelant
  if (r == True)
  then do
    line <- (setVarWithString ("Test   : OK"))
    appendFile strResultFile line 
    
  else do
    line <- (setVarWithString ("Test   : FAILT!"))
    appendFile strResultFile line 

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ testCNF: Test CNF function for t random formulas                          │
-- │ Inputs │ Type │ Description                                               │
-- │ t      │ Int  │ Number of the test                                        │
-- └───────────────────────────────────────────────────────────────────────────┘
testCNF :: Int -> IO()
testCNF t = do
  if (t > test_max )
  then do
    -- number of test wanted is to big
    
    -- write divider line to file
    line <- strDividerLine
    appendFile strResultFile line
    
    -- write reason changed number of test then requested
    line <- (setVarWithString ("Too many tests, only " ++ show test_max ++ " test(s) will be done maximal!"))
    appendFile strResultFile line
    
    -- start testint with max tests
    testCNF test_max
  
  else do
    if (t > 0)
    then do 
      -- recursively call test_isPermutation n times 
      testCNF (t-1)

      -- Place actual test
      f <- getRandomF
      let g = cnf(nnf(arrowfree f))
      let r = equiv f g
      writeCNF_testResult t f g r
      
      -- print progress to the screen
      print ("Test " ++ show t ++ " done")
            
    else do
      -- write first line to file
      line <- strDividerLine
      writeFile strResultFile line
      
      -- Start testing!
      line <- (setVarWithString ("Start Testing...."))
      appendFile strResultFile line


-- exercise 7
-- mehmet: changed the getRandomInts to getRandomChar
getRandomChar :: IO Char
getRandomChar = getStdRandom (randomR ('a','z'))

getRandomFormula :: Int -> IO Formula
getRandomFormula 0 = do 
  m <- getRandomChar
  return (Atom [m] [])

getRandomFormula d = do 
  n <- getRandomInt 5
  case n of
    0 -> do 
      m <- getRandomChar
      return (Atom [m] [])
          
    1 -> do 
      f <- getRandomFormula (d-1)
      return (Neg f)
       
    2 -> do 
      m  <- getRandomInt 5
      fs <- getRandomFormulas (d-1) m
      return (Conj fs)
       
    3 -> do 
      m  <- getRandomInt 5
      fs <- getRandomFormulas (d-1) m
      return (Disj fs)
       
    4 -> do 
      m <- getRandomChar
      f <- getRandomFormula (d-1)
      return (Forall [m] f)
       
    5 -> do 
      m <- getRandomChar
      f <- getRandomFormula (d-1)
      return (Exists [m] f)
       
getRandomFormulas :: Int -> Int -> IO [Formula]
getRandomFormulas _ 0 = return []
getRandomFormulas d n = do
	f <- getRandomFormula d
	fs <- getRandomFormulas d (n-1)
	return (f:fs)



