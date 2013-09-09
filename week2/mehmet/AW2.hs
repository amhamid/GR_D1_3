-- ╔════════╤═════════════════════════════════════════════════════════════════╗
-- ║ Modele │ Assignment Week 2                                               ║
-- ║        │ Lab Session Software Testing 2013, Week 2 With each deliverable,║
-- ║        │ indicate the time spent.                                        ║
-- ╠════════╪═════════════════════════════════════════════════════════════════╣
-- ║ Auteur │ Mehmet Misset                                                   ║
-- ║ Date   │ 9-9-2013                                                        ║
-- ╚════════╧═════════════════════════════════════════════════════════════════╝
-- ─│┌┐└┘├┤┬┴┼═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╪╫╬

module AW2

where

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ Imports                                                                  ║
-- ╚══════════════════════════════════════════════════════════════════════════╝
--import C:\Users\Mehmet\Documents\GitHub\GR_D1_3\week2\week2.hs
-- begin code week 2: moet nog imprt worden
--module Week2

--where 

import Data.List
import Data.Char

data Coin = C Int

w :: Coin -> Float 
w (C n) = if n == lighter then 1 - 0.01
          else if n == heavier then 1 + 0.01
          else 1

weight :: [Coin] -> Float
weight = sum . (map w)

balance :: [Coin] -> [Coin] -> Ordering 
balance xs ys = 
  if weight xs < weight ys then LT
  else if weight xs > weight ys then GT
  else EQ

outcome :: (Float -> Bool) -> (Float, Bool -> Float) 
            -> Float
outcome accept (x,decide) = 
  if accept x then x + (decide True)
              else (1 - x) + (decide False)

jill :: Float -> Bool
jill = \ x -> x > 1/2

joe :: (Float, Bool -> Float)
joe = (2/3, \p -> if p then 1/100000 else 1/2)

jillVariations :: [Float -> Bool]
jillVariations = 
   [ \ x -> x >= y/100 | y <- [50..100] ]

joeVariations :: [(Float, Bool -> Float)]
joeVariations = 
  [(z/100,\ p -> if p then 1/100000000 else 1/2) 
                               | z <- [50..100] ]

testJill1 :: Float
testJill1 = minimum 
  [ outcome ji jo | ji <- jillVariations, 
                    jo <- joeVariations ]

testJill2 :: Float
testJill2 = minimum 
  [ outcome jill jo | jo <- joeVariations ]

testJoe1 :: Float
testJoe1 = 2 - maximum 
  [ outcome ji jo | ji <- jillVariations, 
                    jo <- joeVariations ]

testJoe2 :: Float
testJoe2 = 2 - maximum 
  [ outcome ji joe | ji <- jillVariations ]

run :: Integer -> [Integer]
run n | n < 1 = error "argument not positive"
      | n == 1 = [1]
      | even n = n: run (div n 2)
      | odd n  = n: run (3*n+1)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving Eq

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3

-- form1 = ((1==>2)<=>(-2==>-1))
form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))

-- form2 = ((1==>2)<=>(-1==>-2))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))

-- form3 = (*((1==>2) (2==>3))==>(1==>3))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concat (map pnames fs)
  pnames (Dsj fs) = concat (map pnames fs)
  pnames (Impl f1 f2) = concat (map pnames [f1,f2])
  pnames (Equiv f1 f2) = 
          concat (map pnames [f1,f2])

type Valuation = [(Name,Bool)]

-- all possible valuations for list of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

eval :: Valuation -> Form -> Bool
eval [] (Prop c)    = error ("no info: " ++ show c)
eval ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = eval xs (Prop c)
eval xs (Neg f)  = not (eval xs f)
eval xs (Cnj fs) = all (eval xs) fs
eval xs (Dsj fs) = any (eval xs) fs
eval xs (Impl f1 f2) = 
     not (eval xs f1) || eval xs f2
eval xs (Equiv f1 f2) = eval xs f1 == eval xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

-- no precondition: should work for any formula. 
arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

-- precondition: input is arrowfree
nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

lighter, heavier :: Int
lighter = 3
heavier = 0

-- einde code week 2

-- ╔═════════════════╤════════╤════════════════════════╗
-- ║                 │ Symbol │ Name                   ║
-- ╠═════════════════╪════════╪════════════════════════╣
-- ║ and             │   Λ    │ Conjunction            ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ or              │   V    │ Disjunction            ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ not             │   ¬    │ Negation               ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ if──then        │  ==>   │ Implication            ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ if, and only if │  <=>   │ Equivalence            ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ for all         │        │ Universial Quantifier  ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ for some        │        │ Existential Quantifier ║
-- ╚═════════════════╧════════╧════════════════════════╝

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 1. Write a program (in Haskell) that takes a triple of integer values as ║
-- ║    arguments and gives as output one of the following statements:        ║
-- ║    - `Not a triangle' (`Geen driehoek') if the three numbers cannot      ║
-- ║       occur as the lengths of the sides of triangle,                     ║
-- ║       ( a <= 0) V ( b <= 0 ) V ( c <= 0 )                                ║
-- ║                                                                          ║
-- ║    - `Equilateral' (`Gelijkzijdig') if the three numbers are the lengths ║
-- ║      of the sides of an equilateral triangle,                            ║ 
-- ║       (a == b) Λ ( b == c )                                              ║
-- ║                                                                          ║
-- ║    - `Rectangular' (`Rechthoekig') if the three numbers are the lengths  ║
-- ║      of the sides of a rectangular triangle,                             ║
-- ║      ( a^2 + b^2 ) == ( c^2 )                                            ║
-- ║      ( b^2 + c^2 ) == ( a^2 )                                            ║
-- ║      ( c^2 + a^2 ) == ( b^2 )                                            ║
-- ║                                                                          ║ 
-- ║    - `Isosceles' (`Gelijkbenig') if the three numbers are the lengths of ║
-- ║      the sides of an isosceles (but not equilateral) triangle,           ║   
-- ║      (a == b) Λ (a == ¬c)                                                ║
-- ║      (b == c) Λ (b == ¬a)                                                ║
-- ║      (c == a) Λ (c == ¬b)                                                ║
-- ║                                                                          ║
-- ║    - `Other' (`Anders') if the three numbers are the lengths of the sides║
-- ║      of a triangle that is not equilateral, not rectangular, and not     ║
-- ║      isosceles.                                                          ║
-- ║                                                                          ║
-- ║ Here are some useful type definitions                                    ║
-- ║                                                                          ║ 
-- ║ data Shape = NoTriangle | Equilateral | Isosceles | Rectangular |        ║
-- ║                 Other deriving (Eq,Show)                                 ║
-- ║ triangle :: Integer -> Integer -> Integer -> Shape                       ║ 
-- ║                                                                          ║
-- ║ You may wish to consult http://en.wikipedia.org/wiki/Triangle.           ║
-- ║ Indicate how you tested or checked the correctness of the program.       ║
-- ║ Deliverables: Haskell program, test report of 1/2 A4, indication of time ║
-- ║ spent.                                                                   ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Definition of the type of shape of the triangle                          │
-- └──────────────────────────────────────────────────────────────────────────┘
data Shape = NoTriangle 
           | Equilateral
           | Isosceles 
           | Rectangular
           | Other deriving (Eq,Show)

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Definition of triangle decription                                        │
-- └──────────────────────────────────────────────────────────────────────────┘
triangle a b c = if ((a < 1) || 
                     (b < 1) || 
                     (c < 1)) then NoTriangle

            else if ((a == b) && 
                     (b == c)) then Equilateral

            else if ((( a^2 + b^2 ) == ( c^2 )) ||
                     (( b^2 + c^2 ) == ( a^2 )) ||
                     (( c^2 + a^2 ) == ( b^2 ))) then Rectangular

            else if (((a == b) && (not (a == c))) ||
                     ((b == c) && (not (b == a))) ||
                     ((c == a) && (not (c == b)))) then Isosceles

            else Other

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ 9 Tests are needed, one for every test in the code                       │
-- └──────────────────────────────────────────────────────────────────────────┘
test1_1 = (triangle (-1) 1  0 )  -- NoTriangle
test1_2 = (triangle   1  1  1 )  -- Equilateral
test1_3 = (triangle   3  4  5 )  -- Rectangular
test1_4 = (triangle   4  3  5 )  -- Rectangular
test1_5 = (triangle   5  4  3 )  -- Rectangular
test1_6 = (triangle   1  1  2 )  -- Isosceles
test1_7 = (triangle   1  2  2 )  -- Isosceles
test1_8 = (triangle   2  2  1 )  -- Isosceles
test1_9 = (triangle   1  3  4 )  -- other
test1   = print(test1_1, 
                test1_2, 
                test1_3, test1_4, test1_5,
                test1_6, test1_7, test1_8,
                test1_9)




-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2. The lecture notes of this week discuss the notions of satisfiability, ║
-- ║    tautology, contradiction, logical entailment and logical equivalence  ║
-- ║    for formulas of propositional logic.                                  ║
-- ║    The lecture notes give a definition of satisfiable, for objects of    ║
-- ║    type Form.                                                            ║ 
-- ║    Use a module that imports Week2.hs. Check that your definitions are   ║
-- ║    correct.                                                              ║
-- ║    Deliverables: implementation, description of your method of checking  ║
-- ║    the definitions, indication of time spent.                            ║
-- ║                                                                          ║
-- ║ Your task is to give definitions of:                                     ║
-- ║                                                                          ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ A contradiction is formula that is NOT satisfiabe                        │
-- └──────────────────────────────────────────────────────────────────────────┘
contradiction :: Form -> Bool
contradiction f = not (any (\ v -> eval v f) (allVals f))


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ A Tautology formula that is satisfiabe for all you put into it           │
-- └──────────────────────────────────────────────────────────────────────────┘
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ A logical entailment is P |= Q if and if only P <=> Q                    │
-- └──────────────────────────────────────────────────────────────────────────┘
entails :: Form -> Form -> Bool
entails f g = ((     (tautology  f)  &&      (tautology g)  )  || 
               ((not (tautology  f)) && (not (tautology g) )))
-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ A logical equivalence is .....                                           │
-- └──────────────────────────────────────────────────────────────────────────┘
equiv :: Form -> Form -> Bool 
equiv f g = (tautology  f) == (tautology g)

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 3. The lecture notes of this week discuss the conversion of Boolean      ║
-- ║    formulas (formulas of propositional logic) into CNF form. The lecture ║
-- ║    notes also give a definition of a Haskell datatype for formulas of    ║
-- ║    propositional logic, using lists for conjunctions and disjunctions.   ║
-- ║    Your task is to write a Haskell program for converting formulas into  ║
-- ║    CNF, and to test whether the conversion is correct.                   ║
-- ║                                                                          ║
-- ║    Deliverables: conversion program, implementation of a number of tests,║
-- ║    test report, indication of time spent.                                ║
-- ║                                                                          ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

toCNF :: Form -> Form
toCNF f = nnf (arrowfree f)

-- *AW2> form1
-- ((1==>2)<=>(-2==>-1))
-- *AW2> toCNF form1
-- +(*(+(-1 2) +(2 -1)) *(*(1 -2) *(-2 1)))
-- *AW2> equiv form1 (toCNF form1)
