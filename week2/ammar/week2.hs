module Week2 where

-- importing 'lib/Week2FromLecture.hs' (which is given during the lecture)
import Week2FromLecture


---------------------------------------
-- exercise 1 (Time spent: 1 hour)
---------------------------------------

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
	| (a + b < c) || (a + c < b) || (b + c < a) = NoTriangle
	| (a == b) && (a == c) = Equilateral
	| (a == b) || (a == c) || (b == c) = Isosceles 
	| (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
	| otherwise = Other

-- Testing report:
--
-- We can use algebraic reasoning that the formula above is correct; no need to test it manually.
-- However, we could also prove it by means of contradiction.
--
-- For NoTriangle case, let a = 1, b = 1, c = 100 then it should be a NoTriangle; either you visualize it or actually try to build a real triangle and for sure it won't work.
-- so we can run the following: 
-- 	(triangle 1 1 100 /= NoTriangle) == False and this should return to True
--
-- Using proof by contradiction, we can test this triangle formula.
-- However, because Integer has no maxBound therefore there is no way I cannot manual test it for all inputs.


------------------------------------
-- exercise 2 (Time spent: 3 hours)
------------------------------------

contradiction :: Form -> Bool
contradiction f = all (\ v -> eval v f && eval v (Neg f)) (allVals f)

-- TODO add some explanation



tautology :: Form -> Bool
tautology f = all (\ v -> eval v f || eval v (Neg f)) (allVals f)

-- TODO add some explanation



entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

-- TODO add some explanation



equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> eval v (h)) (allVals (h)) where h = Equiv f g

-- TODO add some explanation



-----------------------------------
-- exercise 3 (Time spent: 1 hour)
-----------------------------------

-- precondition: input is arrowfree
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg(Prop x)) = Neg (Prop x)
cnf (Cnj [f, g]) = Cnj [cnf f, cnf g]
cnf (Dsj [f, g]) = dist (cnf f, cnf g)

-- cnf helper to translate 'P V (Q ^ R)' to '(P V Q) ^ (P V R)' 
-- 	      and also for '(P ^ Q) V R' to '(P V R) ^ (Q V R)'
dist :: (Form, Form) -> Form
dist (Cnj [f,f'], g) = Cnj [dist (f,g), dist(f', g)]
dist (f, Cnj [g, g']) = Cnj [dist (f,g), dist(f, g')]
dist (f, g) = Dsj [f, g]

-- TODO add some explanation





