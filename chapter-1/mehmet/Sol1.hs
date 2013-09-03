
-- +--------------------------------------------------------------------------+
-- | Modele: GS (chapter 1 of book - The Haskell Road to Logic)               |
-- | Auteur: Mehmet Misset                                                    |
-- | Date  : 3-9-2013                                                         |
-- +--------------------------------------------------------------------------+

module GS

where

-- Imports
import Data.List


--
-- exercise 1.1: Calutations
myadd :: Integer -> Integer -> Integer
myadd x y = x + y

mysub :: Integer -> Integer -> Integer
mysub x y = x - y

myprodut :: Integer -> Integer -> Integer
myprodut x y = x * y

mydivide :: Float -> Float -> Float
mydivide x y = x / y


-- exercise 1.2: Prime Number
--myPrime :: Integer -> Bool
--myPrime n | n == 1    = True
--          | n == 2    = True
--          | otherwise = False

-- exercise 1.3: divides
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ld :: Integer -> Integer 
ld n = ldf 2 n

-- exercis 1.4: replacing '>' with '>='
-- yes it would act different the condition k^2 >= 2 is met wher k=1 and n=1 
--  this this will not hold with k^2 > n, bacause 1 > 1.

-- exercise 1.5: Prime0
prime0 :: Integer -> Bool
prime0 n | n  < 1    = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n