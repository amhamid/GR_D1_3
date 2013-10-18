module Week2V2 where

-- importing 'lib/Week2FromLecture.hs' (which is given during the lecture)
import Week2FromLecture

-- Exercise 1
data Shape = NoTriangle 
		   | Equilateral 
		   | Isosceles 
		   | Rectangular 
		   | Other 
		   deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c	| (a+b < c) || 
				  (a+c < b) || 
				  (b+c < a)									= NoTriangle
				| (a == b) && (b == c)						= Equilateral
				| (a == b) || (b == c) || (a == c)			= Isosceles
				| (a^2 + b^2 == c^2) 						= Rectangular
				| otherwise 								= Other
{-
main = do
		print $ triangle 1 1 3
		print $ triangle 3 2 5
		print $ triangle 3 2 1
		print $ triangle 3 3 2
		print $ triangle 3 3 3
		print $ triangle 4 3 5
-}

-- Exercise 2
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

tautology :: Form -> Bool
tautology f = not(contradiction f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = contradiction f1 || satisfiable f2

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 | satisfiable f1 && satisfiable f2		= True
			| contradiction f2 && contradiction f1	= True
			| otherwise								= False

-- Exercise 3
{- The lecture notes of this week discuss the conversion of Boolean for-
mulas (formulas of propositional logic) into CNF form. The lecture
notes also give a definition of a Haskell datatype for formulas of propo-
sitional logic, using lists for conjunctions and disjunctions. Your task
is to write a Haskell program for converting formulas into CNF, and to
test whether the conversion is correct. -}

-- precondition: input is arrowfree
cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg(Prop x)) = Neg (Prop x)
cnf (Neg (Neg (Prop x))) = Prop x
cnf (Cnj xs) = Cnj (map cnf xs)
cnf (Dsj xs) = Dsj (map cnf xs)


































