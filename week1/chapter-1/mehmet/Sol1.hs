-- +--------------------------------------------------------------------------+
-- | Modele: GS G-etting S-tarted (CH 1 of book - The Haskell Road to Logic)  |
-- | Auteur: Mehmet Misset                                                    |
-- | Date  : 3-9-2013                                                         |
-- +--------------------------------------------------------------------------+

module GS

where

-- Imports
-- 

-- +--------------------------------------------------------------------------+
-- | 1.1 Starting Haskell Interpreter                                         |
-- +--------------------------------------------------------------------------+

--
-- Exercise 1.1: Calutations
myadd :: Integer -> Integer -> Integer
myadd x y = x + y

mysub :: Integer -> Integer -> Integer
mysub x y = x - y

myprodut :: Integer -> Integer -> Integer
myprodut x y = x * y

mydivide :: Float -> Float -> Float
mydivide x y = x / y


-- +--------------------------------------------------------------------------+
-- | 1.2 Implementing a Prime Number Test                                     |
-- +--------------------------------------------------------------------------+

-- Exercise 1.2: Prime Number
--myPrime :: Integer -> Bool
--myPrime n | n == 1    = True
--          | n == 2    = True
--          | otherwise = False

-- Exercise 1.3: divides
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ld :: Integer -> Integer 
ld n = ldf 2 n

-- Exercise 1.4: replacing '>' with '>='
-- yes it would act different the condition k^2 >= 2 is met wher k=1 and n=1 
--  this this will not hold with k^2 > n, bacause 1 > 1.

-- Exercise 1.5: Prime0
prime0 :: Integer -> Bool
prime0 n | n  < 1    = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n


-- +--------------------------------------------------------------------------+
-- | 1.3 Haskell Type Declaration                                             |
-- +--------------------------------------------------------------------------+

-- Exercise 1.6: type def rem
-- rem :: Integer -> Integer -> Integer

-- Exercise 1.7: type def checking

-- *GS> :t divides 5
-- divides 5 :: Integer -> Bool

-- *GS> :t divides 5 7
-- divides 5 7 :: Bool


-- +--------------------------------------------------------------------------+
-- | 1.4 Identifiers in Haskell                                               |
-- +--------------------------------------------------------------------------+


-- +--------------------------------------------------------------------------+
-- | 1.5 Playing the Haskell Game                                             |
-- +--------------------------------------------------------------------------+

-- Exercise 1.8: minimum of list of Integers
mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)

-- Exercise 1.9: maximum of list of Integers
mxmInt :: [Int] -> Int
mxmInt []     = error "empty list"
mxmInt [x]    = x
mxmInt (x:xs) = max x (mxmInt xs)

-- *GS> mxmInt [1..123]
-- 123

-- *GS> mxmInt [1, 4,5,7234, 213, 123345]
-- 123345

-- Exercise 1.10: remove First
-- need more thinking
myCompare :: Int -> Int -> [Int]
myCompare m x | m == x    = []
              | otherwise = [x]

merge :: [Int] -> [Int] -> [Int]
merge []     []     = []
merge [xs]   []     = [xs]
merge []     [ys]   = [ys]
merge []     (y:ys) = y : ys
merge (x:xs) []     = x : xs
merge (x:xs) (y:ys) = x : y : merge xs ys

removeFst :: Int -> [Int] -> [Int]
removeFst m []     = []
removeFst m [x]    = myCompare m x
removeFst m (x:xs) = merge (myCompare m x) (removeFst m xs )

-- found this ad http://stackoverflow.com/questions/14688716/removing-the-first-instance-of-x-from-a-list
deleteFirst :: Int -> [Int] -> [Int]
deleteFirst m (x:xs) | m == x    = xs 
                     | otherwise = x : deleteFirst m xs

-- Exercise 1.11: Sorting
-- didn't got 1.10
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs



-- Exercise: 1.12
average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum' xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs 


length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs


-- Exercise: 1.13: Count
myCount :: String -> String -> Int
myCount c []     = 0
myCount c (x:xs) = 1 + (length' ( filter (==c) [xs] ))

addone :: Int -> Int
addone m = m + 1

myrepeat :: String -> Int -> String
myrepeat c n | n <  0    = ""
             | n == 0    = ""
             | otherwise = c ++ (myrepeat c (n-1))


-- Exercise 1.14: blowup
blowup :: String -> String
blowup x  | x == []   = "" 
          | otherwise = (blowup (reverse (tail (reverse x)))) ++ (myrepeat [(last x)] (length x)) 

-- Exercise 1.15: sort string
delFstChr :: Char -> String -> String
delFstChr c (x:xs) | c == x    = xs 
                   | otherwise = x : delFstChr c xs

mnmChr :: String -> Char
mnmChr []     = error "empty list"
mnmChr [x]    = x
mnmChr (x:xs) = min x (mnmChr xs)

srtString :: String -> String
srtString [] = []
srtString xs = m : (srtString (delFstChr m xs)) where m = mnmChr xs

-- Exercise 1.16: prefix
myPrefix :: String -> String -> Bool
myPrefix str1 str2 | (length str1) > (length str2)     = False
                   | str1 == (take (length str1) str2) = True
                   | otherwise                         = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (y==x) && prefix xs ys

-- Exercise: 1.17: SubString
subString :: String -> String -> Bool
subString [] ys =True
subString (x:xs) [] = False
subString (x:xs) (y:ys) = (y==x) && prefix xs ys || subString (x:xs) ys


-- +--------------------------------------------------------------------------+
-- | 1.6 Haskell Types                                                        |
-- +--------------------------------------------------------------------------+

-- Exercise: 1:18: Find expersions
-- 1. [String]
-- 2. (Bool, String)
-- 3. [(Bool, String)]
-- 4. ([Bool], String)
-- 5. Bool -> Bool

-- Don't understand what the meaning of thes exercise

-- Exercise: 1.19: interpeter
-- 1. head      ==>> *GS> :t head
--                   head :: [a] -> a

-- 2. last      ==>> *GS> :t last
--                   last :: [a] -> a

-- 3. init      ==>> *GS> :t init
--                   init :: [a] -> [a]

-- 4. fst       ==>> *GS> :t fst
--                   fst :: (a, b) -> a

-- 5. (++)      ==>> *GS> :t (++)
--                   (++) :: [a] -> [a] -> [a]

-- 6. flip      ==>> *GS> :t flip
--                   flip :: (a -> b -> c) -> b -> a -> c

-- 7. filp (++) ==>> *GS> :t flip (++)
--                   flip (++) :: [a] -> [a] -> [a]


-- +--------------------------------------------------------------------------+
-- | 1.7 The Prime Factorization Algorithm                                    |
-- +--------------------------------------------------------------------------+


-- +--------------------------------------------------------------------------+
-- | 1.8 The map and Filter functions                                         |
-- +--------------------------------------------------------------------------+

-- Exercise: 1.20: lengths
lengths :: [[a]] -> [Int]
lengths []  = []
lengths [x] = map length [x]
lengths xs  = map length xs

-- Exercise: 1.21: lengths
sumlengths :: [[a]] -> Int
sumlengths []  = 0
sumlengths [x] = sum' (lengths [x])
sumlengths xs  = sum' (lengths xs)

-- Exercise: 1.22: improving all primes
primes0 :: [Integer]
primes0 = filter prime0 [2..]

-- Exercise: 1.23: improving LD
ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n

--Exercise: 1.24: can you explain?

--no 




















