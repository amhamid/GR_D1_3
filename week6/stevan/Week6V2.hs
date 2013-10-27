module Week6V2

where
import Data.List
import Data.Char
import System.Random
import Week6FromLecture

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Q1
{-
0	Leaf
	
1	Node
  t1	t2
  
2		  Node
	Node		Node
  t1	t2	  t1	t2

3					  Node
		  Node					  Node
	Node		Node		Node		Node
  t1	t2	  t1	t2	  t1	t2	  t1	t2

4								  	  Node
				   Node									 Node
		 Node	  			 Node			   Node				   Node
	Node	  Node		Node	  Node	  Node		Node	  Node	  	Node
  t1	t2	t1	  t2  t1	t2	t1	t2	t1	  t2  t1	t2  t1	  t2  t1   t2
-}

-- Q2
data Btree a = Leaf a 
			 | Node (Btree a) (Btree a) 
			 deriving (Eq,Show)

-- Give a formula for the number of nodes in a balanced binary tree of depth n. Next, show by structural induction that your formula is correct
numberOfNodes :: Btree a -> Int
numberOfNodes (Leaf _) = 0
numberOfNodes (Node t1 t2) = numberOfNodes t1 + numberOfNodes t2 + 1

-- numberOfNodes (Node (Leaf "") (Leaf ""))
-- numberOfNodes (Node (Node (Node (Leaf "") (Leaf "")) (Leaf "")) (Node (Leaf "") (Leaf "")))
-- Next, show by structural induction that your formula is correct






-- Q3
-- Define a function leafCount :: Btree a -> Int that counts the number of leaf nodes in a binary tree.
leafCount :: Btree a -> Int
leafCount (Leaf _) = 1
leafCount (Node t1 t2) = leafCount t1 + leafCount t2

-- leafCount (Leaf "")
-- leafCount (Node (Leaf "") (Leaf ""))
-- leafCount (Node (Node (Node (Leaf "") (Leaf "")) (Leaf "")) (Node (Leaf "") (Leaf "")))

-- How can you test leafCount for correctness? Or can you perhaps prove that it is correct?





-- Q4
-- Define a function mapB that does for binary trees what map does for lists.
mapB :: (a -> b) -> Btree a -> Btree b
mapB f (Leaf a) = Leaf (f a)
mapB f (Node t1 t2) = Node (mapB f t1) (mapB f t2)

exampleTree = Node (Node (Leaf "hoare, tony") (Leaf "turing, alan")) (Leaf "goedel, kurt")
-- mapB (map toUpper) exampleTree



-- Q5
data Tree a = T a [Tree a] 
			deriving (Eq,Ord,Show)

example0 = T 2 []
example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]
-- Define a function count :: Tree a -> Int that counts the number of nodes of a tree.
count :: Tree a -> Int
count (T a []) = 1
count (T a (x:xs)) = count x + count (T a xs)

-- How can you test count for correctness? Or can you perhaps prove that it is correct?




-- Q6
depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1
-- How can you test depth for correctness? Or can you perhaps prove that it is correct?







-- Q7
{-
mapB :: (a -> b) -> Btree a -> Btree b
mapB f (Leaf a) = Leaf (f a)
mapB f (Node t1 t2) = Node (mapB f t1) (mapB f t2)

data Tree a = T a [Tree a] 
			deriving (Eq,Ord,Show)
-}
-- Define a function mapT that does for trees what map does for lists.
-- Hint: in the definition you will need both map and mapT.
mapT :: (a -> b) -> Tree a -> Tree b
--mapT f (T a []) = T (f a) []
-- mapT f (T a (x:xs)) = T (f a) []
-- mapT f (T a [T b [], T c []]) = T (f a) []
-- mapT f (T a b) = T (f a) []
-- mapT f (T a (x:xs)) = T (f a) (map (mapT f x) xs)
mapT f (T a xs) = T (f a) (map (mapT f) xs)



-- example0 = T 2 []
-- mapT succ example0
-- T 3 []

-- example1 = T 1 [T 2 [], T 3 []]
-- mapT succ example1
-- T 2 [T 3 [],T 4 []]

-- example2 = T 0 [example1,example1,example1]
-- mapT succ example2
-- T 1 [T 2 [T 3 [],T 4 []],T 2 [T 3 [],T 4 []],T 2 [T 3 [],T 4 []]]












































