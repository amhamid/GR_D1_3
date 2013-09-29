module Week4 where

import SetOrd
import System.Random

-- I don't understand how to solve Exercise 4.47 of Ch 4. I can't seem to store lists in temp variables,
-- and I don't think I am supposed to store lists in temp variables.

-- Exercise 4.46
reverse' :: [a] -> [a]
reverse' []  = []
reverse' [x] = [x]
reverse' xs  = last xs : reverse' (take (length xs-1) xs)

-- Exercise 4.47
splitIn2 :: [a] -> [a] -> ([a],[a])
splitIn2 _ [] = ([], [])
splitIn2 (ts) (x:xs) = ((ts), (x:xs))

-- [([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4])]
splitList :: [a] -> [([a],[a])]
splitList []  = []
splitList xxs@(x:xs) = ( [x],xs ) : splitList xs


-- 
--list in
--split into n-1 pairs

--[1..n] -> [ ([1],[2..n]) .. ([1..n-1],[n]) n-1 ]

--the split2set should be called n-1 times

--split2set xs
--xxs@(x:xs)

-- Exercise 2
{-
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getListRandomInt :: Int -> Int -> [IO Int]
getListRandomInt n x = print (getRandomInt x : getListRandomInt (n-1) x)
-}



type Rel a = [(a,a)]

infir 5 @@

--(@@) :: Eq a => Rel a -> Rel a -> Rel a
r (@@) s = 
	nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
	
	

















						  
						  

























