module LabExam

where
import Data.List
import Assert

f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

data BinTree a = Nil | B a (BinTree a) (BinTree a) deriving (Eq,Show)

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value  (_,y) = y 

-- Question 1
--fA = assert1 (\ (s,r) (t,u) -> ... == ... ) f

-- Question 2
-- No answer

-- Question 3
-- No answer

-- Question 4
-- No answer

-- Question 5
-- No answer

-- Question 6
-- No answer
