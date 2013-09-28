module AW4

where

-- My Own Solutions
import AW2
import AW3

-- Colleges
import Week2 hiding (Neg)
import Week3
import Week4

-- Extra's
import Techniques
import SetOrd

-- usefull stuff
import Data.List
import System.Random

-- +--------------------------------------------------------------------------+
-- | 2. Implement a random data generator for the datatype Set Int, where     |
-- |    Set is as defined in http://homepages.cwi.nl/~jve/rcrh/SetOrd.hs.     |
-- |    (Deliverables: Haskell program, indication of time spent.)            |
-- +--------------------------------------------------------------------------+

-- Function genSet generates a set of 'random' integers.
-- Time spent: 2 hours
genSet :: IO (Set Int)
genSet = do
  s <- genIntSet 10
  return (Set s)

-- Max Size of Set
maxSizeSet = 999

-- precondition n must be filled with number
genIntSet :: Int -> IO [Int]
genIntSet n = do 
  if (n > 1000) then do
    print ("only using 1.000 random generated elements")
    let n = 1000
    getRandomSetElements maxSizeSet n
  else do
    getRandomSetElements maxSizeSet n

    
getRandomSetElements :: Int -> Int -> IO [Int]
getRandomSetElements _ 0 = return []
getRandomSetElements d n = do 
  f  <- getRandomInt d
  fs <- getRandomSetElements d (n-1) 
  return (insertSet' (f+1) fs)
  
insertSet' :: (Ord a) => a -> [a] -> [a] 
insertSet' x y = insertList' x y

insertList' x [] = [x]
insertList' x ys@(y:ys') = 
  case compare x y of 
    GT -> y : insertList' x ys' 
    EQ -> ys 
    _  -> x : ys   
    
-- +--------------------------------------------------------------------------+
-- | 3. Implement operations for set intersection, set union and set          |
-- |    difference, for the datatype Set defined in                           |
-- |    http://homepages.cwi.nl/~jve/rcrh/SetOrd.hs. Next, use automated      |
-- |    random testing to check that your implementation is correct.          |
-- |    Note: you may have to change import List to import Data.List in the   |
-- |    module SetOrd. (Deliverables: Haskell program, test code, short test  |
-- |    report, indication of time spent.)                                    |
-- +--------------------------------------------------------------------------+

-- intersection time spent 1 hour
intersection :: (Ord a) => Set a -> Set a -> Set a 
intersection  (Set [])     set2    = (Set [])
intersection   set1       (Set []) = (Set [])
intersection  (Set (x:xs)) set2    =  
  if (inSet x set2)
  then insertSet x (intersection (Set xs) set2)
  else intersection (Set xs) set2

-- union basicly the same as the one in SetOrd.hs  
union' :: (Ord a) => Set a -> Set a -> Set a 
union' (Set [])     set2  =  set2
union' (Set (x:xs)) set2  = 
   insertSet x (union' (Set xs) set2)

-- difference time spent 1/2 hour
difference :: (Ord a) => Set a -> Set a -> Set a 
difference  (Set [])     set2    = (Set [])
difference   set1       (Set []) = (Set [])
difference  (Set (x:xs)) set2    =  
  if (inSet x set2)
  then deleteSet x set2
  else insertSet x (difference (Set xs) set2)


-- test report time spent 1/2 hour
test_set1 = Set [1,2,3]     -- Set 1
test_set2 = Set [3,4,5]     -- Set 2
test_set3 = Set [3]         -- Set 3 is intersection of Set 1 and 2
test_set4 = Set [1,2,3,4,5] -- Set 4 is union of Set 1 and 2
test_set5 = Set [1,2,4,5]   -- Set 5 is difference of Set 1 and 2

-- short test report of intersection
test_intersection :: IO()
test_intersection = do
  let test = "Intersection"
  let result = intersection test_set1 test_set2
  print_test test test_set1 test_set2 result (result == test_set3)
  
-- short test report of union
test_union :: IO()
test_union = do
  let test = "Union"
  let result = union' test_set1 test_set2
  print_test test test_set1 test_set2 result (result == test_set4)
  
-- -- short test report of difference
test_difference :: IO()
test_difference = do
  let test = "Difference"
  let result = difference test_set1 test_set2
  print_test test test_set1 test_set2 result (result == test_set5)
  
testreport :: IO ()
testreport = do
  print ("================================================")
  print ("Testing Intersection, Union and Difference with:")
  print ("================================================")
  test_intersection
  print ("------------------------------------------------")
  test_union
  print ("------------------------------------------------")
  test_difference
  print ("------------------------------------------------")
  print ("Done")

print_test :: (Show a) => String -> Set a -> Set a-> Set a -> Bool -> IO ()
print_test t setA setB setR r = do  
  print ("Testing " ++ t ++ " with:")
  print ("Set A: " ++ show setA)
  print ("Set B: " ++ show setB)
  print (t ++ " of Set A and Set B " ++ show setR)
  if (r == True)
  then  print ("Test: OK")
  else  print ("Test: FAILT!")
  
-- +--------------------------------------------------------------------------+
-- | 4. Suppose we implement binary relations as list of pairs, Haskell type  |
-- |    [(a,a)].                                                              |
-- |                                                                          |
-- |    Assume the following definitions:                                     | 
-- |                                                                          |
-- |    type Rel a = [(a,a)]                                                  |
-- |                                                                          |
-- |    infixr 5 @@                                                           |
-- |                                                                          |
-- |    (@@) :: Eq a => Rel a -> Rel a -> Rel a                               |
-- |                                                                          |
-- |    r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]               |
-- |                                                                          |
-- |    Use this to implement a function                                      |
-- |                                                                          |
-- |    trClos :: Ord a => Rel a -> Rel a                                     |
-- |                                                                          |
-- |    that gives the transitive closure of a relation, where the relation   |
-- |    is represented as a list of pairs.                                    |
-- |    E.g., trClos [(1,2),(2,3),(3,4)] should give                          |
-- |    [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].                                |
-- |                                                                          |
-- |   (Deliverable: Haskell program, indication of time spent.)              |
-- +--------------------------------------------------------------------------+
type Rel a = [(a,a)]

infixr 5 @@
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

test_set6 = [(1,2),            (2,3),      (3,4)] -- R
test_set7 = [      (1,3),            (2,4)      ] -- R^2  
test_set8 = [            (1,4)                  ] -- R^2 o R = R^3  
test_set9 = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] -- UNION of R and R^2 and R^3

r_on_r :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a,a)]
r_on_r []          []          = []
r_on_r [(x,y)]     []          = []
r_on_r []          [(u,v)]     = []
r_on_r ((x,y):xys) []          = []
r_on_r []          ((u,v):uvs) = []
r_on_r ((x,y):xys) ((u,v):uvs) =
  if y == u
  then addPairToSetOfPairs (x, v) ((r_on_r [(x,y)] uvs) ++ (r_on_r xys ((u,v):uvs)))
  else (r_on_r [(x,y)] uvs) ++ (r_on_r xys ((u,v):uvs)) 

r_n :: (Ord a) => [(a, a)] -> Int -> [(a,a)]
r_n []      _ = []
r_n [(x,y)] n = [(x,y)]
r_n xys     n | (n == 0)  = error("n must be greater the 0!")
              | (n == 1)  = xys
              | otherwise = r_on_r xys (r_n xys (n-1))

r_union :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
r_union []          []          = []
r_union [(x,y)]     []          = [(x,y)]
r_union []          [(u,v)]     = [(u,v)]
r_union ((x,y):xys) []          = [(x,y)] ++ xys
r_union []          uvs         =  uvs
r_union ((x,y):xys) uvs  = 
   addPairToSetOfPairs (x,y) (r_union xys uvs)
  
-- trClos :: Ord a => Rel a -> Rel 
trClos' :: (Ord a) => [(a,a)] -> [(a,a)]
trClos' []  = []
trClos' xys = r_kp xys 1000

r_kp :: (Ord a) => [(a,a)] -> Int -> [(a,a)]
r_kp xys n | (n > 2)  = 
              if ((r_n xys n) /= [])
              then r_kp (r_union xys (r_n xys n)) (n-1)
              else r_kp (r_union xys (r_n xys n)) 2
           | otherwise = r_union xys (r_n xys 2)

-- precondition (x,y) must be filled
-- precondition (u,v) must be filled  
combine2pairs :: (Ord a) => (a,a) -> (a,a) -> [(a,a)]
combine2pairs (x,y) (u,v) = 
  case compare (x,y) (u,v) of 
    GT -> [(u,v)] ++ [(x,y)]
    EQ -> [(x,y)]
    LT -> [(x,y)] ++ [(u,v)] 
  
addPairToSetOfPairs :: (Ord a) => (a,a) -> [(a,a)] -> [(a,a)]
addPairToSetOfPairs (x,y) []          = [(x,y)]
addPairToSetOfPairs (x,y) uvs@((u,v):uvs') = 
  case compare (x,y) (u,v) of 
    GT -> [(u,v)] ++ addPairToSetOfPairs (x,y) uvs'
    EQ -> uvs
    LT -> [(x,y)] ++ uvs
    
-- +--------------------------------------------------------------------------+
-- | 5. Test the function trClos from the previous exercise. Devise your own  |
-- |    test method for this. Try to use random test generation. Define       |
-- |    reasonable properties to test.                                        |
-- |   (Deliverables: test code, short test report, indication of time spent.)|
-- +--------------------------------------------------------------------------+


