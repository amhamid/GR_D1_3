module Week2 
(Shape, triangle) 

where

-- importing 'lib/Week2FromLecture.hs' (which is given during the lecture)
import Week2FromLecture


---------------
-- exercise 1
---------------

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
	| (a+b<c) || (a+c<b) || (b+c<a) = NoTriangle
	| (a == b) && (a == c) = Equilateral
	| (a == b) || (a == c) || (b == c) = Isosceles 
	| (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
	| otherwise = Other


-- TODO indicate how I should test/check the correctness of the program

-- Integer has no maxBound so pratically I cannot test for all inputs
-- The truth table size is (number of Shape)^(number of inputs), so in this case 5^3, which is 125 rows.
--



-- TODO indicate time spent






---------------
-- exercise 2
---------------

-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> eval v f) (allVals f)


contradiction :: Form -> Bool
contradiction f = all (\v -> not(eval v f)) (allVals f)




