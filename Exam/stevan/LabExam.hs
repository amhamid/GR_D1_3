module LabExam

where
import Data.List
import Assert

f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value  (_,y) = y 

-- Question 1
{-
input (0,30)
ouput (1,15)
                (0,30) (1,15)    -}
{-			
pre 
	invariant
post

assert (pre, post)

P ==> Q  (~P V Q)
T     T   T
T	  F   F
F     T   T
F     F   T
								P				Q -}
fA = assert1 (\ (s,r) (t,u) -> not (s == 0) || (r == 2^t * u) ) f
-- fA = assert1 (\ (s,r) (t,u) -> (s == 0) ==> (r == 2^t * u) ) f  -- ==> is defined in Assert.hs




-- Question 2
-- BinTree trees have no information at their leaf nodes, but do have information at their internal nodes
data BinTree a = Nil 
			   | B a (BinTree a) (BinTree a) 
			   deriving (Eq,Show)

-- Btree trees have information at their leaf nodes, but do not have information at their internal nodes
data Btree a = Leaf a 
			 | Node (Btree a) (Btree a) 
			 deriving (Eq,Show)
-- Implement a function bintree2btree :: a -> BinTree a -> Btree a that converts a BinTree to a Btree by throwing away internal node information and insertion of copies of the first argument at the leaves. So bintree2btree x should insert copies of x at the leaf nodes.
{-
BinTree
  a
 b c
Nil Nil  --> Leaf x   Leaf x

Btree
  0
 0 0
xx xx
-}
bintree2btree :: a -> BinTree a -> Btree a
-- replace Nil with Leaf x
bintree2btree x Nil = Leaf x
-- create btree for left branch and for the right branch
bintree2btree x (B _ t1 t2) = Node (bintree2btree x t1) (bintree2btree x t2)


-- Implement a function btree2bintree :: a -> Btree a -> BinTree a that converts a Btree to a BinTree by throwing away the leaf information, and filling up the internal nodes with copies of the first argument. So btree2bintree x should insert copies of x at the internal nodes.
btree2bintree :: a -> Btree a -> BinTree a
-- replace Leaf x with Nil
btree2bintree _ (Leaf _) = Nil
-- create BinTree for left branch and for right branch
btree2bintree x (Node t1 t2) = B x (btree2bintree x t1) (btree2bintree x t2)

-- Show how you can test these two functions for correctness




-- Question 3
{-
data BinTree a = Nil 
			   | B a (BinTree a) (BinTree a) 
			   deriving (Eq,Show)
-}
-- In-order traversal of a binary tree visits the nodes of the tree by first doing an in-order traversal of the left subtree, then visiting the root node, and next doing an in-order traversal of the right subtree. Implement a function
inOrder :: BinTree a -> [a]
-- When we reach Nil an empty array is returned 
inOrder Nil = []
-- Visit the left Node, then the root, and then the right Node
inOrder (B a t1 t2) = (inOrder t1) ++ [a] ++ (inOrder t2)


{-
        12
	 
  10			20

5     11	 13		25

Nil Nil      Nil Nil

(B 12 (B 10 (B 5 Nil Nil) (B 11 Nil Nil)) (B 20 (B 13 Nil Nil) (B 25 Nil Nil)))

[5,10,11,12,13,20,25]
-}



-- that collects the items found by in-order traversal of a binary tree in a list. Next define a second function for in-order traversal in right to left direction:
inOrderRev :: BinTree a -> [a]
-- When we reach Nil an empty array is returned
inOrderRev Nil = []
-- Visit the right Node, then the root, and then the left Node
inOrderRev (B a t1 t2) = (inOrderRev t2) ++ [a] ++ (inOrderRev t1)

{-
        12
	 
  10			20

5     11	 13		25

Nil Nil      Nil Nil

(B 12 (B 10 (B 5 Nil Nil) (B 11 Nil Nil)) (B 20 (B 13 Nil Nil) (B 25 Nil Nil)))

[25,20,13,12,11,10,5]
-}


-- Finally, fill in the dots to define a property that can be used to test the two functions by relating them to each other.
treeProperty :: Eq a => BinTree a -> Bool
-- 
treeProperty t = inOrder t == reverse (inOrderRev t)




-- Question 4
{-
type Dict = BinTree (String,String)

key, value :: (String,String) -> String
key (x,_) = x
value (_,y) = y
-}
ordered :: Dict -> Bool
ordered a = inOrder a == sort (inOrder a) 

--ordered a = let a' = inOrder a
--			in a' == sort a'

{-
         ("j", "bla1")
		 
	("e", "bla2")       ("m", "bla3)

B ("j", "bla1") (B ("e", "bla2") Nil Nil)(B ("m", "bla3") Nil Nil)

sort [("j", "bla1"), ("e", "bla2"), ("m", "bla3)]
-}





-- Question 5
{-
data BinTree a = Nil 
			   | B a (BinTree a) (BinTree a) 
			   deriving (Eq,Show)

type Dict = BinTree (String,String)

key, value :: (String,String) -> String
key (x,_) = x
value (_,y) = y
-}
-- Implement a function lookUp :: String -> Dict -> [String] that looks up a key in an ordered dictionary. Make sure the lookup function exploits the order. An output [] indicates that the key is not defined in the dictionary, a non-empty list gives the value for a given key. Recall that the items in the dictionary tree have the form (key,value). You can assume each key occurs at most once in the dictionary.
-- Implement a function lookUp ::
lookUp :: String -> Dict -> [String]
-- that looks up a key in an ordered dictionary. Make sure the lookup function exploits the order.
-- An output [] indicates that the key is not defined in the dictionary
lookUp s (B (key, val) Nil Nil) = if s == key then [val] else []
lookUp s (B (key, val) t1 Nil) = if s == key then [val] ++ lookUp s (t1) else lookUp s (t1)
lookUp s (B (key, val) Nil t2) = if s == key then [val] ++ lookUp s (t2) else lookUp s (t2)
-- a non-empty list gives the value for a given key
lookUp s (B (key, val) t1 t2) = if s == key then [val] ++ lookUp s (t1) ++ lookUp s (t2) else lookUp s (t1) ++ lookUp s (t2)
-- lookUp "y" (B ("y", "yes") Nil Nil)
-- lookUp "s" (B ("s", "sexy") (B ("yy", "you") (B ("y", "yes") Nil Nil) Nil) (B ("s", "beast!") Nil Nil))


-- Implement a function that looks up a key in an ordered dictionary. Make sure the lookup function exploits the order.
lookUp' :: String -> Dict -> [String]
lookUp' s d = theHelp s (inOrder d)
--lookUp' s d = let d' = inOrder d
--			  in theHelp s d'

theHelp :: String -> [(String, String)] -> [String]
theHelp s [] = []
theHelp s [([], [])] = []
theHelp s [(key, [])] = []
theHelp s [([], value)] = []
theHelp s [(key, value)] = if s == key then [value] else []
theHelp s ((key, value):xs) = if s == key then [value] ++ theHelp s xs else theHelp s xs
-- lookUp' "y" (B ("y", "yes") Nil Nil)
-- lookUp' "s" (B ("s", "sexy") (B ("yy", "you") (B ("y", "yes") Nil Nil) Nil) (B ("s", "beast!") Nil Nil))








-- Question 6
-- No answer



{-

post decomp

-- decompPost = post1 (\ (r,s) -> even r) (decomp) 

decompPre = pre1 (\ x -> x == 100) decomp

mult 100 10

decompInv = invar1 (\ x -> x == 100) mult

decompPost 30  = 

decomp === decompPost

decomp x = (a,b)
decomp 30 = (1,15) --> 15 odd

30 (0,30)
30 (1,15)






assert1 (\ n (k,m) -> n == 2ˆk*m) decomp



-}



































