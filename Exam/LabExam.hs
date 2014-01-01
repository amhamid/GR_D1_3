module LabExam

where
import Data.List
import Assert

-- by Ammar Hamid (10463593)

-----------------
-- Question 1
-----------------
f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

fA = assert1 (\ (s,r) (t,u) -> (s==0) && (odd u) ==> (r==2^t * u)) f

{-
Function f is basically a decomposition function such that f (0,r) = (t,u) where r = 2^t * u
This is true only when 0 is used in f(0,_).
Since this function ask input (s,r) then we need to assert three things:
1. we assert that the input (s == 0)
2. we assert that the result (u is odd)
3. If both are true then it implies that (r = 2^t * u)

I use implication in the assertion because f is not really created for decomposition, if it is created
for that then I would assume that the function signature will be f :: Integer -> (Integer,Integer). And
if that is the case then I would just check for (odd u) && (r==2^t * u)
-}


-----------------
-- Question 2
-----------------
data BinTree a = Nil | B a (BinTree a) (BinTree a) deriving (Eq,Show)

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

bintree2btree :: a -> BinTree a -> Btree a
bintree2btree x Nil = Leaf x
bintree2btree x (B _ t1 t2) = Node (bintree2btree x t1) (bintree2btree x t2)

{-
example:

*LabExam> bintree2btree 5 (B 10 Nil Nil)
 Node (Leaf 5) (Leaf 5)

*LabExam> bintree2btree 5 (B 10 (B 20 Nil Nil) (B 20 Nil Nil))
 Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf 5) (Leaf 5))
-}

btree2bintree :: a -> Btree a -> BinTree a
btree2bintree _ (Leaf _) = Nil
btree2bintree x (Node t1 t2) = B x (btree2bintree x t1) (btree2bintree x t2)

{-
example:

*LabExam> btree2bintree 10 (Node (Leaf 5) (Leaf 5))
 B 10 Nil Nil

*LabExam> btree2bintree 10 (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf 5) (Leaf 5)))
 B 10 (B 10 Nil Nil) (B 10 Nil Nil)

These functions can be tested by structural induction.
For example: 
The base case of bintree2btree is to replace Nil with a Leaf x (where x is the information)
The induction hypothesis is that we can also do this with a BinTree with a certain depth n=k
From here, we can also prove for n=k+1 by converting the left branch and the right branch.

This way this implementation in Haskell is already a proof by itself.

We could also by the way test this by converting two times using different function and then 
compare them and the result should be the same with original one.

example:
bintree1 = B 10 Nil Nil
btree1 = bintree2btree bintree1
btree2bintree btree1 == bintree1
-}


-----------------
-- Question 3
-----------------
inOrder :: BinTree a -> [a]
inOrder Nil = []
inOrder (B x t1 t2) = (inOrder t1) ++ [x] ++ (inOrder t2)

{-
example:

*LabExam> inOrder (B 5 (B 1 Nil Nil) (B 10 Nil Nil))
 [1,5,10]
-}

inOrderRev :: BinTree a -> [a]
inOrderRev Nil = []
inOrderRev (B x t1 t2) = (inOrderRev t2) ++ [x] ++ (inOrderRev t1)

{-
example:

*LabExam> inOrderRev (B 5 (B 1 Nil Nil) (B 10 Nil Nil))
 [10,5,1]
-}

treeProperty :: Eq a => BinTree a -> Bool
treeProperty t = inOrder t == reverse (inOrderRev t)

{-
example:

*LabExam> inOrder (B 5 (B 1 Nil Nil) (B 10 Nil (B 20 Nil Nil)))
 [1,5,10,20]

*LabExam> inOrderRev (B 5 (B 1 Nil Nil) (B 10 Nil (B 20 Nil Nil)))
 [20,10,5,1]

*LabExam> treeProperty (B 5 (B 1 Nil Nil) (B 10 Nil (B 20 Nil Nil)))
 True
-}


-----------------
-- Question 4
-----------------
type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value (_,y) = y 

ordered :: Dict -> Bool
ordered dict = let
					xs = inOrder dict
				in
					xs == sort xs

{-
example:

*LabExam> ordered (B ("g","bla1") (B ("a","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
 True

*LabExam> ordered (B ("a","bla1") (B ("g","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
 False
-}					


-----------------
-- Question 5
-----------------
lookUp :: String -> Dict -> [String]
lookUp _ Nil = []
lookUp keyword (B (key, value) t1 t2) = 
	let
		comparison = compare keyword key
	in
		if comparison == EQ then [value]
		else if comparison == LT then lookUp keyword t1
		else lookUp keyword t2

{- 
example:

*LabExam> lookUp "a" (B ("g","bla1") (B ("a","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
 ["bla2"]

*LabExam> lookUp "j" (B ("g","bla1") (B ("a","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
 ["bla4"]
-}


-----------------
-- Question 6
-----------------
insertKey :: (String,String) -> Dict -> Dict
insertKey (newKey, newValue) Nil = B (newKey, newValue) Nil Nil
insertKey (newKey, newValue) (B (key, value) t1 t2) = 
	let
		comparison = compare newKey key
	in
		if comparison == EQ then B (newKey, newValue) t1 t2
		else if comparison == LT then B (key, value) (insertKey (newKey, newValue) t1) t2
		else B (key, value) t1 (insertKey (newKey, newValue) t2)

{- 
example:

*LabExam> insertKey ("b", "bla5") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla2") (B ("b","bla5") Nil Nil) Nil) (B ("h","bla3") Nil (B ("j","bla4") Nil Nil))

*LabExam> insertKey ("i", "bla6") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h","bla3") Nil (B ("j","bla4") (B ("i","bla6") Nil Nil) Nil))

*LabExam> insertKey ("c", "bla10") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla10") Nil Nil) (B ("h","bla3") Nil (B ("j","bla4") Nil Nil))
-}
	

insertKeyA = assert2 (\ _ dict newDict -> ordered dict ==> ordered newDict) insertKey

{-
insertKeyA makes sure that if before insertion the dictionary is sorted then after insertion the new dictionary must also be sorted.

example:

*LabExam> insertKeyA ("b", "bla5") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla2") (B ("b","bla5") Nil Nil) Nil) (B ("h","bla3") Nil (B ("j","bla4") Nil Nil))

*LabExam> insertKeyA ("i", "bla6") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h","bla3") Nil (B ("j","bla4") (B ("i","bla6") Nil Nil) Nil))

*LabExam> insertKeyA ("c", "bla10") (B ("g","bla1") (B ("c","bla2") Nil Nil) (B ("h", "bla3") Nil (B ("j", "bla4") Nil Nil)))
B ("g","bla1") (B ("c","bla10") Nil Nil) (B ("h","bla3") Nil (B ("j","bla4") Nil Nil))
-}

