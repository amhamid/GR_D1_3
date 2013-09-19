module Week2 where

-- importing 'lib/Week2FromLecture.hs' (which is given during the lecture)
import Week2FromLecture


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 1 - Triangle (Time spent: 1 hour)                               │
-- └──────────────────────────────────────────────────────────────────────────┘

data Shape = NoTriangle 
           | Equilateral
           | Isosceles 
           | Rectangular
           | Other deriving (Eq,Show)

-- precondition: a, b, c should be bigger or equal to 1
triangle :: Integer -> Integer -> Integer -> Shape
-- VVZ: correct, but too much hassle to write down all three permutations - why not sort the triples first and then check the sorted list?
triangle a b c
	 | a < 1 || b < 1 || c < 1 = error "should be positive number bigger or equal than 1"
         | (a + b < c) || (a + c < b) || (b + c < a) = NoTriangle
         | (a == b) && (a == c) = Equilateral
         | (a == b) || (a == c) || (b == c) = Isosceles
         | (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
         | otherwise = Other

-- Testing report:
--
-- We can use algebraic reasoning that the formula above is correct; no need to test it manually.
-- VVZ: still, testing is possible!
-- However, we could also prove it by means of contradiction.
--
-- For NoTriangle case, let a = 1, b = 1, c = 100 then it should be a NoTriangle; either you visualize it or actua    lly try to build a real triangle and for sure it won't work.
-- so we can run the following: 
--      (triangle 1 1 100 /= NoTriangle) == False and this should return to True
--
-- However, because Integer has no maxBound therefore there is no way I cannot manual test it for all inputs.
-- VVZ: you could write a Haskell function to generate it!

-- This is what we get so far:

-- Total number of test cases will be 9! 9 because every next test case, following the
-- order(NoTriangle -> Equilateral -> Isoceles -> Rectangular -> Other) eliminates the
-- need for additional test cases.
-- VVZ: there are many definitions of "the need for additional test cases" ;)
-- VVZ: the one you are using is "condition coverage": you want to cover every clause in every Boolean expression
-- But if you really want to do all the test cases, you have 38 possible test cases.
--
-- NoTriangle has 1 variable	2^1
-- Equilateral has 2 variables	2^2
-- Isoceles has 3 variables	2^3
-- Rectangular has 3 variables	2^3
-- Other has 4 variables	2^4
--						
-- Total test cases		38
-- 
-- NoTriangle
-- If and only if any of the values are below 1 then it is not a triangle
--  p <=> NoTriangle
--  0       0
--  1       1
--
-- Equilateral
-- Precondition: Is a triangle
-- If and only if all the values are equal then it is an Equilateral triangle
--  p && q  :=  Equilateral
--  0    0  		0
--  0    1      	0
--  1    0      	0
--  1    1      	1
--
-- Isoceles
-- Precondition: NOT Equilateral
-- If and only if two of the three values are equal then it is an Isoceles triangle
--  p || q || r    :=  Isoceles
--  0    0    0              0
--  0    0    1      	     1
--  0    1    0      	     1
--  0    1    1     	     0
--  1    0    0              1
--  1    0    1     	     0
--  1    1    0              0
--  1    1    1 	     0
--
-- Rectangular
-- Precondition: NOT Isoceles
-- If and only if a^2 + b^2 = c^2 then it is a Rectangular triangle
-- (p || q || r) :=  Rectangular
--  0    0    0           0
--  0    0    1           1
--  0    1    0           1
--  0    1    1           0
--  1    0    0           1
--  1    0    1           0
--  1    1    0           0
--  1    1    1           0
--
-- Other
-- Any values that don't conform to one of the previous test case will do
-- Triangle && NOT(Equilateral) && NOT(Isoceles) && NOT(Rectangular) :=  Other
--    1              1                   1                 1               1



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2 (Time spent: 2 hours)                                         │
-- └──────────────────────────────────────────────────────────────────────────┘

-- A contradiction is formula that is NOT satisfiable
contradiction :: Form -> Bool
-- VVZ: correct, but a shorter notation would be "contradiction = not . satisfiable"
contradiction f = not (satisfiable f)

-- A Tautology formula that is satisfiabe for all you put into it
tautology :: Form -> Bool
tautology f = not (contradiction f)

-- f has g as logical consequence/entailment if and only if 'f -> g' is a tautology
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

-- f is equivalence to g if only if all valuations of f is equal to all valuations of g
equiv :: Form -> Form -> Bool 
equiv f g = all (\ v -> eval v (h)) (allVals (h)) where h = Equiv f g
-- VVZ: could it also be just "entails f g && entails g f"?



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 3 - CNF (Time spent: 1 hour)                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- precondition: input is arrowfree and nnf
-- VVZ: incorrect, counterexample: "cnf (Neg (Neg p))"
-- VVZ: another counterexample: "cnf (Cnj [Cnj [p,q], q])"
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg(Prop x)) = Neg (Prop x)
-- VVZ: also only defined for binary conjunction/disjunction
cnf (Cnj [f, g]) = Cnj [cnf f, cnf g]
cnf (Dsj [f, g]) = dist (cnf f, cnf g)
 
-- cnf helper to translate 'P V (Q ^ R)' to '(P V Q) ^ (P V R)' 
--            and also for '(P ^ Q) V R' to '(P V R) ^ (Q V R)'
dist :: (Form, Form) -> Form
dist (Cnj [f,f'], g) = Cnj [dist (f,g), dist(f', g)]
dist (f, Cnj [g, g']) = Cnj [dist (f,g), dist(f, g')]
dist (f, g) = Dsj [f, g]
 
-- Testing:
 
-- after converting Form to CNF then the result should be equivalent with original Form
-- VVZ: not just that! it should also actually be in CNF!
-- as an example below: we convert form1 to CNF and make a comparison between the original and its convertion. (al    so for form2 and form3)
testCnf = and ( map (\ f -> equiv (cnf (nnf(arrowfree f))) f) [form1, form2, form3] )

