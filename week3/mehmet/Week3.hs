module AW3

where

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║ Imports                                                                   ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝
import AW2
import Week2 hiding (Neg) -- thanx to Ammar!
import Week3
import Techniques
import Data.List
import System.Random


-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 3. Consult the course slides of this week to write a generator for random │
-- │    integer lists.                                                         │
-- └───────────────────────────────────────────────────────────────────────────┘
genIntList :: IO [Int]
genIntList = do n <- getRandomInt 10
                d <- getRandomInt 10
                getRandomItems d n

getRandomItems :: Int -> Int -> IO [Int]
getRandomItems _ 0 = return []
getRandomItems d n = do 
                     f  <- getRandomInt d
                     fs <- getRandomItems d (n-1) 
                     return (f:fs)

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 4. A permutation of a finite list is another finite list with the same    │
-- │    elements, but possibly in a different order. For example, [0,2,0] is a │
-- │    permutation of [0,0,2], but [2,2,0] is not. Write a function           │
-- └───────────────────────────────────────────────────────────────────────────┘
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []    []     = False
isPermutation [x]   []     = False
isPermutation []    [y]    = False
isPermutation [xs]  [ys]    = any ([x]==) (permutations [y])
isPermutation (x:xs) (y:ys) = any ((x:xs)==) (permutations (y:ys))

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 5. Define some testable properties for this function, and use your random │
-- │    generator for integer lists from Exercise 3 to test isPermutation.     │
-- └───────────────────────────────────────────────────────────────────────────┘
test_max = 30
test_lenght_of_IntList = 4

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ A testable property is the length of the input array's                    │
-- │ if the lenght of input array's differces isPermutation can never be true  │
-- └───────────────────────────────────────────────────────────────────────────┘
-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Input t is number of tests                                                │
-- │ Input l is True for variable lengte of array's                            │
-- └───────────────────────────────────────────────────────────────────────────┘
test_isPermutation :: Int -> Bool -> IO ()
test_isPermutation t l = do
  if (t > test_max )
  then do
    print ("===========================================================================")
    print ("Too many tests, only " ++ show test_max ++ " test(s) will be done maximal!")

    -- call test_isPermutation with test_max
    test_isPermutation test_max l

  else do
    if (t > 0)
    then do 
      -- recursively call test_isPermutation n times 
      test_isPermutation (t-1) l

      -- Place actual test
      if (l == True)
      then do
        x <- genIntList
        y <- genIntList
        show_test t x y (isPermutation x y)

      else do
        v <- (genIntListOfN test_lenght_of_IntList)
        w <- (genIntListOfN test_lenght_of_IntList)
        show_test t v w (isPermutation v w)

      -- Print te test result
      
      print ("===========================================================================")

    else do
      print ("===========================================================================")	
      if (l == True)
      then print ("Start Testing with variable array lenghts")
      else print ("Start Testing with static array lengths")
      print ("===========================================================================")	


show_test :: Show a => Int -> [a] -> [a] -> Bool -> IO()
show_test n x y r = do
  if (n > 99)
  then print   ("Test " ++ show n ++ ": isPermuation " ++ show x ++ " " ++ show y)  
  else 
    if (n < 10)
    then print ("Test   " ++ show n ++ ": isPermuation " ++ show x ++ " " ++ show y)  
    else print ("Test  " ++  show n ++ ": isPermuation " ++ show x ++ " " ++ show y)  

  if (r == True)
  then print ("Test OK")
  else do 
    if ((length x) == (length y))
    then print ("Test Failt!: y is no permutation x")
    else print ("Test Failt!: y has a diffent lenght then x")
    
-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 6. Use the random formula generator from the Techniques slides to test    │
-- │    your CNF program of last week. (Deliverable: file with tests for CNF   │
-- │    program, report on the results).                                       │
-- └───────────────────────────────────────────────────────────────────────────┘

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
-- │ testCNF: Test CNF function for 10 random formulas                         │
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

      -- write divider line to file
      line <- strDividerLine
      appendFile strResultFile line
          
-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ 7. Write a random formula generator for formulas of First Order Logic     │
-- │    (as defined in the Week 3 course slides).                              │                                     │
-- └───────────────────────────────────────────────────────────────────────────┘
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
