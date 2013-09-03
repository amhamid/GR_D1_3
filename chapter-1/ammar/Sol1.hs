module Sol1 where
	
import GS
import Data.List

-- exercise 1.3
mydivides :: Integer -> Integer -> Bool
mydivides d n = rem n d == 0
 

-- exercise 1.5
myprime0 :: Integer -> Bool
myprime0 n | n < 1     = error "not a positive integer"
           | n == 1    = False 
           | otherwise = ld n == n


-- exercise 1.6
-- to find out type of rem then use: 
-- :t rem
-- rem :: Integral a => a -> a -> a


-- exercise 1.9
maximumInt :: [Integer] -> Integer
maximumInt []     = error "empty list"
maximumInt [x]    = x
maximumInt (x:xs) = max x (maximumInt xs) 


-- exercise 1.10
removeFst :: Integer -> [Integer] -> [Integer]
removeFst m xs = filter (/=m) xs 


-- exercise 1.13
count :: Char -> String -> Int
count c s = length (filter (==c) s)


-- exercise 1.14 
-- For the sake of my understanding and readability, here is a note about how it works:
-- first: zip [1..] "bang" will produce [(1,'b'),(2,'a'),(3,'n'),(4,'g')]
-- then for (1, 'b') : take 1 (repeat 'b')
-- then for (2, 'a') : take 2 (repeat 'a') ---> repeat will actually repeat arg infinitely, hence the 'take' function to actually stop at certain number
-- then for (3, 'n') : take 3 (repeat 'n')
-- then for (4, 'g') : take 4 (repeat 'g')
-- and the result will be assign to x = "b aa nnn gggg" (no space, I gave a space just for to make it clear)
blowup :: String -> String
blowup xs = [ x | (n,y) <- zip [1..] xs , x <- take n (repeat y)]


-- exercise 1.15
sortString :: [String] -> [String]
-- TODO consider re-implementing Data.List.sort for my better understanding of functional programming
sortString xs = sort xs 


-- exercise 1.17
-- redefine prefix function, just for my understanding
myprefix :: String -> String -> Bool
myprefix [] ys = True
myprefix xs [] = False
myprefix (x:xs) (y:ys) = (x==y) && myprefix xs ys

substring :: String -> String -> Bool
substring [] ys = True
substring xs [] = False
substring (x:xs) (y:ys) = ((x==y) && prefix xs ys) || substring (x:xs) ys



