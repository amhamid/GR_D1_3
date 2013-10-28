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

-- My code looks way prettier and concise then exM, so can I already conclude that its more efficient? :)
-- For testing:
{-
http://en.wikipedia.org/wiki/Modular_exponentiation#Memory-efficient_method
e' = 1. c = (1 * 4) mod 497 = 4 mod 497 = 4.			== exM3 4 1 497
e' = 2. c = (4 * 4) mod 497 = 16 mod 497 = 16.			== exM3 4 2 497
e' = 3. c = (16 * 4) mod 497 = 64 mod 497 = 64.			== exM3 4 3 497
e' = 4. c = (64 * 4) mod 497 = 256 mod 497 = 256.		== exM3 4 4 497
e' = 5. c = (256 * 4) mod 497 = 1024 mod 497 = 30.		== exM3 4 5 497
e' = 6. c = (30 * 4) mod 497 = 120 mod 497 = 120.		== exM3 4 6 497
e' = 7. c = (120 * 4) mod 497 = 480 mod 497 = 480.		== exM3 4 7 497
e' = 8. c = (480 * 4) mod 497 = 1920 mod 497 = 429.		== exM3 4 8 497
e' = 9. c = (429 * 4) mod 497 = 1716 mod 497 = 225.		== exM3 4 9 497
e' = 10. c = (225 * 4) mod 497 = 900 mod 497 = 403.		== exM3 4 10 497
e' = 11. c = (403 * 4) mod 497 = 1612 mod 497 = 121.	== exM3 4 11 497
e' = 12. c = (121 * 4) mod 497 = 484 mod 497 = 484.		== exM3 4 12 497
e' = 13. c = (484 * 4) mod 497 = 1936 mod 497 = 445.	== exM3 4 13 497
-}














































