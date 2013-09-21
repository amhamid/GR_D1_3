module AW3

where

-- ╔═══════════════════════════════════════════════════════════════════════════╗
-- ║ Imports                                                                   ║
-- ╚═══════════════════════════════════════════════════════════════════════════╝
import AW2
import Week3
import Techniques
import Data.List


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
