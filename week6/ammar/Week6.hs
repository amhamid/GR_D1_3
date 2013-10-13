module Week6

where
import Data.List
import System.Random
import Week6FromLecture


-- exercise 1
-- this is already given in the Week6FromLecture.


-- exercise 2
-- exM outperforms expM.


-- exercise 3
composites :: [Integer]
composites = composites' [2..]

composites' (x:xs) = if (isPrime x) then composites' xs
	 	    else x : composites' xs


-- exercise 4
-- when k = 561 then it slip through the Fermat's Primality test


-- exercise 5
-- done with reading wiki and some experiments
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


-- exercise 6
-- Miller-Rabin tests are correctly identifying prime number and also catches issue with Fermat's test with Carmichael numbers.


-- exercise 7
-- We can use this test: 
-- 	primeMR x == primeMR (2^x - 1)
-- So if x is prime and 2^x-1 is also prime then x is a Mersenne prime number


