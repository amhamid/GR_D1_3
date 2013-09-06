-- Compile a .hs file
--
-- C:\>cd C:\Users\stevan\Desktop\UvA\Software Testing\Week 1
-- C:\..\>ghci
-- Prelude> :! ghc -c lib/GS.hs
-- Prelude> :! ghc -c lib/TAMO.hs
-- Prelude> :q

-- Start GHCi with a lib dir included and load your Program/Script
--
-- C:\>cd C:\Users\stevan\Desktop\UvA\Software Testing\Week 1
-- C:\..\>ghci -ilib/
-- Prelude> :l chapter-1/stevan/Sol1

-- Show currently loaded modules
--
-- *Sol1> :show modules
-- Sol1             ( chapter-1\stevan\Sol1.hs, interpreted )
-- GS               ( lib\GS.hs, lib\GS.o )

module Sol1 where

import GS

-- Exercise 1.3
mydivides :: Integer -> Integer -> Bool
mydivides d n = rem n d == 0

-- Exercise 1.5
myld :: Integer -> Integer
myld n = myldf 2 n

myldf :: Integer -> Integer -> Integer
myldf k n	| mydivides k n	= k
			| k^2 > n		= n
			| otherwise		= myldf (k+1) n
			
myprime0 :: Integer -> Bool
myprime0 n	| n < 1		= error "Not a positive integer"
			| n == 1	= False
			| otherwise	= myld n == n
			
-- Exercise 1.6
-- Added Type Declaration to functions

-- Exercise 1.7
-- divides 5 :: Integer - > Bool
-- The resulting input value should be of the type Integer.
--
-- divides 5 7 :: Bool
-- The resulting value from the function divides is of type Boolean.

-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- Exercise 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst _ []			 		= []
removeFst m (x:xs)	| m == x	= xs
					| otherwise = x : removeFst m xs

-- Exercise 1.13
count :: Char -> String -> Int
count c [x] 	| c == x	= 1
				| otherwise = 0
count c (x:xs)	| c == x	= 1 + count c xs
				| otherwise	= 0 + count c xs

-- Exercise 1.14
blowup :: String -> String
blowup x = concatMap g (zip x [1..]) 
			where g (x,y) = replicate y x

-- Exercise 1.15
-- srtString :: [String] -> [String]
-- srtString [x] = sort [x]
-- I give up...

-- Exercise 1.17
-- substring :: String -> String -> Bool
-- substring xs ys		| xs == ys			= True
-- 						| isInfixOf xs ys	= True
-- I give up...

-- Exercise 1.18
-- Sure...

-- Exercise 1.19
-- ...

-- Exercise 1.20

-- Exercise 1.21

-- Exercise 1.24
-- No...

-- I spent about 8 hours on chapter 1 and didnt manage to finish it properly.
-- Next is chapter 2 and then there is a PaperSession...
-- I am very much starting to worry about my available timeframe and understanding of Haskell.












