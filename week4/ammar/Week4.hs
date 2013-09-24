module Week4 where

import SetOrd
import System.Random
import TechniquesWeek3FromLecture


-- exercise 2
--
-- precondition is d >= n
-- d is the range of random number
-- n is how many elements in the set
genRandomSetInt' :: Int -> Int -> Set Int -> IO (Set Int)
genRandomSetInt' d n (Set s) | d < n = return (error "the first argument must be bigger or equal than the second argument")
                             | length s /= n =  do x <- getRandomInt d
                                                   Set xs <- genRandomSetInt' d n (insertSet x (Set s))
                                                   return (Set xs)
                             | otherwise = return (Set s)

genRandomSetInt :: IO (Set Int)
genRandomSetInt = genRandomSetInt' 100 10 emptySet



-- exercise 3


              