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
-- For NoTriangle case, let a = 1, b = 1, c = 100 then it should be a NoTriangle; either you visualize it or actua    lly try to build a real triangle and for sure it won't work.
-- so we can run the following: 
--      (triangle 1 1 100 /= NoTriangle) == False and this should return to True
--
-- However, because Integer has no maxBound therefore there is no way I cannot manual test it for all inputs.



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2 (Time spent: 2 hours)                                         │
-- └──────────────────────────────────────────────────────────────────────────┘

-- A contradiction is formula that is NOT satisfiable
contradiction :: Form -> Bool
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



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 3 - CNF (Time spent: 1 hour)                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- precondition: input is arrowfree and nnf
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg(Prop x)) = Neg (Prop x)
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
-- as an example below: we convert form1 to CNF and make a comparison between the original and its convertion. (al    so for form2 and form3)
testCnf = and ( map (\ f -> equiv (cnf (nnf(arrowfree f))) f) [form1, form2, form3] )

