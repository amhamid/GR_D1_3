module AW2

where

import Week2

-- Exercise 1
data Shape  = NoTriangle
            | Equilateral
            | Isosceles
            | Rectangular
            | Other deriving Show

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = if (a < 1 || b < 1 || c < 1)                                           then NoTriangle
                 else if (a == b) && (b == c)                                           then Equilateral
                 else if b == c || a == c || a == b                                     then Isosceles
                 else if (a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2) then Rectangular
                 else Other
                 
-- Time spent: reading up on the wikipedia page (30min)
--             + writing and testing code (30min)
--             + writing test repport (90min)
--             = 
--
-- Test Repport
-- Total number of test cases will be 9! 9 because every next test case, following the
-- order(NoTriangle -> Equilateral -> Isoceles -> Rectangular -> Other) eliminates the
-- need for additional test cases.
-- But if you really want to do all the test cases, you have 38 possible test cases.
--
-- NoTriangle has 1 variable	2^1
-- Equilateral has 2 variables	2^2
-- Isoceles has 3 variables		2^3
-- Rectangular has 3 variables	2^3
-- Other has 4 variables		2^4
--								----
-- Total test cases				38
-- 
-- NoTriangle
-- If and only if any of the values are below 1 then it is not a triangle
-- !p <=> NoTriangle
--  0       1
--  1       0
--
-- Equilateral
-- If and only if all the values are equal then it is an Equilateral triangle
--  p && q && !NoTriangle <=> Equilateral
--  0    0       1				0
--  0    1       1				0
--  1    0       1				0
--  1    1       1				1
--
-- Isoceles
-- If and only if two of the three values are equal then it is an Isoceles triangle
--  p q r ((p && q) || (q && r) || (p && r)) && !NoTriangle <=> Isoceles
--  0 0 0     0           0           0           1               0
--  0 0 1     0           0           0           1               0
--  0 1 0     0           0           0           1               0
--  0 1 1     0           1           0           1               1
--  1 0 0     0           0           0           1               0
--  1 0 1     0           0           1           1               1
--  1 1 0     1           0           0           1               1
--  1 1 1     1           1           1           0               0
--
-- Rectangular
-- If and only if a^2 + b^2 = c^2 then it is a Rectangular triangle
-- (p || q || r) && !Isoceles <=> Rectangular
--  0    0    0       1             0
--  0    0    1       1             1
--  0    1    0       1             1
--  0    1    1       0             0
--  1    0    0       1             1
--  1    0    1       0             0
--  1    1    0       0             0
--  1    1    1       0             0
--
-- Other
-- Any values that don't conform to one of the previous test case will do
-- !NoTriangle && !Equilateral && !Isoceles && !Rectangular <=> Other
--    1              1               1            1               1
--
--  main = print (  
--              triangle (-1) 4 4,  -- NoTriangle
--              triangle 0 4 4,     -- NoTriangle
--              triangle 2 2 2,     -- Equilateral
--              triangle 2 4 4,     -- Isoceles
--              triangle 4 4 2,     -- Isoceles
--              triangle 4 2 4,     -- Isoceles
--              triangle 3 4 5,     -- Rectangular
--              triangle 3 5 4,     -- Rectangular
--              triangle 5 4 3,     -- Rectangular
--              triangle 1 2 3      -- Other
--             )
--
--
--------------------------------------------------------------------------------



-- Exercise 2
-- Time spent: answering the question (20min) 
--             + reading Ch 2 of "the haskell Road" (30min) 
--             + reading of "Propositional Logic" www.logicinaction.org/docs/ch2.pdf (180min)
--             + reading slides "understanding the code" (30 min)
--             = 260min
--
-- Contradiction & tautology
-- I don't understand the code in Week2.hs I can not answer this question in Haskell code.
-- But I do know that, the negation of a satisfiable formula, is a contradiction and a negation of
-- a contradiction is a tautology...
--
-- !satisfiable <=> contradiction
-- !contradiction <=> tautology
--
-- Logical entailment
-- No idea how to translate this into Haskell code but, a Logical entailment is to say that if phi then psi 
-- that is that: phi ==> psi
-- which follows that: p ==> q
-- and follows that: !p || q
-- 
-- phi ==> psi
--  0   1   1
--  0   1   1
--  1   0   0
--  1   1   1
--
-- p q ==> !p || q
-- 0 0        1   
-- 0 1        1   
-- 1 0        0   
-- 1 1        1   
--
-- Logical equivalence
-- No idea how to translate this into Haskell code but,
-- a Logical equivalence is to say that if and only if phi then psi 
-- that is that: phi |= psi OR phi <=> psi
-- which follows that: p <=> q
-- and follows that: (!p || q) && (p || !q)
--
-- phi |= psi
--  0  1  0
--  0  0  1
--  1  0  0
--  1  1  1
--
-- p q (!p || q) && (p || !q)
-- 0 0     1     1     1
-- 0 1     1     0     0
-- 1 0     0     0     1
-- 1 1     1     1     1
--
--------------------------------------------------------------------------------



-- Exercise 3










