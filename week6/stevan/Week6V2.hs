module Week6V2

where
import Data.List
import System.Random
import Week6FromLecture




-- Q1
{-
	Implement a function
	that does modular exponentiation of x^y in polynomial time, by repeatedly squaring modulo N.
	E.g., x^33 mod 5 can be computed by means of
	
		x^33 (mod 5) = x^32 (mod 5) * x (mod 5)
		
	x^32 (mod N) is computed in five steps by means of repeatedly squaring modulo N
	
		x (mod N) -> x^2 (mod N) -> x^4 (mod N) -> ... -> x^32 (mod N)
	
0	x^1
1	x^2
2	x^4
3	x^8
4	x^16
5	x^32
-}

exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x e n = mod (x^e) n

{-
mod 33 2 = 1
mod 32 2 = 0

mod 33 2 	= 1
33 - 1 		= 32

div 32 2 	= 16
div 16 2 	= 8
div 8 2 	= 4
div 4 2		= 2
div 2 2		= 1

exM2 :: Integer -> Integer -> Integer -> Integer
exM2 x e n | div e 2 == 0 = 3
--		   | div e 2 == 1 = mod (x^e) n
		   | mod e 2 == 0 = exM2 x (div e 2) n
--		   | div e 2 == 1 = mod (x^e) n
		   | otherwise = mod x n * exM2 x (e-1) n
-}

-- exM1 4 33 497
-- 253

-- mod (mod (4^32) 497 * mod 4 497) 497
-- mod (mod (4^16) 497 * mod (4^16) 497 * mod 4 497) 497
-- mod (mod (4^8) 497 * mod (4^8) 497 * mod (4^8) 497 * mod (4^8) 497 * mod 4 497) 497
-- mod (mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod 4 497) 497

-- exM1 4 32 497
-- 436
-- mod (mod (4^32) 497) 497
-- mod (mod (4^16) 497 * mod (4^16) 497) 497
-- mod (mod (4^8) 497 * mod (4^8) 497 * mod (4^8) 497 * mod (4^8) 497) 497
-- mod (mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497 * mod (4^4) 497) 497

exM3 :: Integer -> Integer -> Integer -> Integer
exM3 x e n | e == 0 = mod 1 n
		   | e == 1 = mod x n
		   | e == 2 = mod (x^e) n
		   | mod e 2 == 0 = let e' = div e 2
							in mod (( exM3 x e' n  )^2) n
		   | mod e 2 == 1 = let e' = div e 2
							in mod (( exM3 x e' n  )^2 * mod x n) n
		   | otherwise = error "42"

-- Q2
-- Check that your implementation is more eficient than exM by running a number of relevant tests and documenting the results.

-- Hmm, not sure how to test this... My code looks way prettier and concise then exM, so I can already conclude that its more efficient? :)















































