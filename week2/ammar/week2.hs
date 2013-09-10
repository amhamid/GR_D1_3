module Week2 
(Shape, triangle) 
where

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)


-- exercise 1

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
	| (a+b<c) || (a+c<b) || (b+c<a) = NoTriangle
	| (a == b) && (a == c) = Equilateral
	| (a == b) || (a == c) || (b == c) = Isosceles 
	| (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
	| otherwise = Other

-- TODO indicate how I should test/check the correctness of the program




