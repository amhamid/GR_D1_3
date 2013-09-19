-- ╔════════╤═════════════════════════════════════════════════════════════════╗
-- ║ Modele │ TAMO T-alking A-bout M-athematical O-bjects                     ║
-- ║        │     (CH 2 of book - The Haskell Road to Logic)                  ║
-- ╠════════╪═════════════════════════════════════════════════════════════════╣
-- ║ Auteur │ Mehmet Misset                                                   ║
-- ║ Date   │ 6-9-2013                                                        ║
-- ╚════════╧═════════════════════════════════════════════════════════════════╝
-- ─│┌┐└┘├┤┬┴┼═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╪╫╬
-- VVZ: just fancifying it further with unicode disjunction and conjunction ;)

module TAMO

where
-- use hugs -98
-- or use ghci -XFlexibleInstances
-- goto command prompt, in windows cmd
-- type in ghci -XFlexibleInstances

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ Imports                                                                  ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.1 Logical Connectives and their Meanings                               ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

-- ╔═════════════════╤════════╤════════════════════════╗
-- ║                 │ Symbol │ Name                   ║
-- ╠═════════════════╪════════╪════════════════════════╣
-- ║ and             │   ∧    │ Conjunction            ║
-- ╟─────────────────┼────────┼────────────────────────╢
-- ║ or              │   ∨    │ Disjunction            ║
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

-- ╔═══════════╗  
-- ║ Negation  ║  
-- ║ not       ║  
-- ╠═════╤═════╣
-- ║  P  │ ¬P  ║
-- ╠═════╪═════╣
-- ║  T  │  F  ║
-- ╟─────┼─────╢
-- ║  F  │  T  ║
-- ╚═════╧═════╝

-- ╔═══════════════════╗
-- ║ Conjunction       ║
-- ║ &&                ║
-- ╠═════╤═════╤═══════╣
-- ║  P  │  Q  │ P ∧ Q ║
-- ╠═════╪═════╪═══════╣
-- ║  T  │  T  │   T   ║
-- ╟─────┼─────┼───────╢
-- ║  T  │  F  │   F   ║
-- ╟─────┼─────┼───────╢
-- ║  F  │  T  │   F   ║
-- ╟─────┼─────┼───────╢
-- ║  F  │  F  │   F   ║
-- ╚═════╧═════╧═══════╝

-- ╔═══════════════════╗
-- ║ Disjunction       ║
-- ║ ||                ║
-- ╠═════╤═════╤═══════╣
-- ║  P  │  Q  │ P ∨ Q ║
-- ╠═════╪═════╪═══════╣
-- ║  T  │  T  │   T   ║
-- ╟─────┼─────┼───────╢
-- ║  T  │  F  │   T   ║
-- ╟─────┼─────┼───────╢
-- ║  F  │  T  │   T   ║
-- ╟─────┼─────┼───────╢
-- ║  F  │  F  │   F   ║
-- ╚═════╧═════╧═══════╝

-- ╔════════════════════╗
-- ║ if──then           ║
-- ║ ==>                ║
-- ╠═════╤═════╤════════╣
-- ║  P  │  Q  │ P => Q ║
-- ╠═════╪═════╪════════╣
-- ║  T  │  T  │   T    ║
-- ╟─────┼─────┼────────╢
-- ║  T  │  F  │   F    ║
-- ╟─────┼─────┼────────╢
-- ║  F  │  T  │   T    ║
-- ╟─────┼─────┼────────╢
-- ║  F  │  F  │   T    ║
-- ╚═════╧═════╧════════╝
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

-- ╔═════════════════════╗
-- ║ if, and only if     ║
-- ║ <=>                 ║
-- ╠═════╤═════╤═════════╣
-- ║  P  │  Q  │ P <=> Q ║
-- ╠═════╪═════╪═════════╣
-- ║  T  │  T  │   T     ║
-- ╟─────┼─────┼─────────╢
-- ║  T  │  F  │   F     ║
-- ╟─────┼─────┼─────────╢
-- ║  F  │  T  │   F     ║
-- ╟─────┼─────┼─────────╢
-- ║  F  │  F  │   T     ║
-- ╚═════╧═════╧═════════╝
infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.2: truth table of exclusice version of or                     │
-- └──────────────────────────────────────────────────────────────────────────┘
-- Don't get this, my swag is not (P ∨ Q) this would result in the table below
-- ╔══════════════════════╗ swag=scientific wild ass guess
-- ║ Disjunction          ║
-- ║ not (P || Q)         ║
-- ╠═════╤═════╤══════════╣
-- ║  P  │  Q  │ ¬(P ∨ Q) ║
-- ╠═════╪═════╪══════════╣
-- ║  T  │  T  │   F      ║
-- ╟─────┼─────┼──────────╢
-- ║  T  │  F  │   F      ║
-- ╟─────┼─────┼──────────╢
-- ║  F  │  T  │   F      ║
-- ╟─────┼─────┼──────────╢
-- ║  F  │  F  │   T      ║
-- ╚═════╧═════╧══════════╝
-- VVZ: NOR ≠ XOR
-- VVZ: XOR is the exclusive or that give T for (T,F) and (F,T) and F otherwise
-- VVZ: it has a great property that P XOR P ≡ T which has many uses in assembly programming

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.4: check truth table of exclusice version of or to  ¬(P<=>Q)  │
-- └──────────────────────────────────────────────────────────────────────────┘
-- Don't get this, my swaq is, but must be doning something wrong
-- ╔══════════════════════╗
-- ║ ¬(P<=>Q)             ║
-- ╠═════╤═════╤══════════╣
-- ║  P  │  Q  │ ¬(P<=>Q) ║
-- ╠═════╪═════╪══════════╣
-- ║  T  │  T  │    F     ║
-- ╟─────┼─────┼──────────╢
-- ║  T  │  F  │    T     ║
-- ╟─────┼─────┼──────────╢
-- ║  F  │  T  │    T     ║
-- ╟─────┼─────┼──────────╢
-- ║  F  │  F  │    T     ║
-- ╚═════╧═════╧══════════╝
infix 1 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Formula1                                                                 │
-- └──────────────────────────────────────────────────────────────────────────┘

p = True
q = False

formula1 = (not p) && (p ==> q) <=> not (q && ( not p))

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Formula2                                                                 │
-- └──────────────────────────────────────────────────────────────────────────┘

formula2 p q = ((not p) && (p ==> q) <=> not (q && ( not p)))

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Valid1                                                                   │
-- └──────────────────────────────────────────────────────────────────────────┘

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Excluded Middle                                                          │
-- └──────────────────────────────────────────────────────────────────────────┘

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Valid2                                                                   │
-- └──────────────────────────────────────────────────────────────────────────┘

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = (bf True  True )
         && (bf True  False)
         && (bf False True )
         && (bf False False)

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Example 2.6:The first law of De Morgan                                   │
-- └──────────────────────────────────────────────────────────────────────────┘

-- ┌──────────────────────────────┐
-- │ ¬ (P  ∧  Q)  (¬  P  ∨  ¬  Q) │
-- ├──────────────────────────────┤
-- │ F  T  T  T    F  T  F  F  T  │
-- │ T  T  F  F    F  T  T  T  F  │
-- │ T  F  F  T    T  F  T  F  T  │
-- │ T  F  F  T    T  F  T  T  F  │
-- └──────────────────────────────┘

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.2 Logical Validity And Related Notations                               ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

class TF p where 
  valid :: p -> Bool
  lequiv :: p -> p -> Bool

instance TF Bool
 where
  valid  = id
  lequiv f g = f == g

instance TF p => TF (Bool -> p)
 where
  valid f = valid (f True) && valid (f False)
  lequiv f g = (f True) `lequiv` (g True)
               && (f False) `lequiv` (g False)

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.11: Tests                                                     │
-- └──────────────────────────────────────────────────────────────────────────┘

test1  = lequiv id (\ p -> not (not p))

test2a = lequiv id (\ p -> p && p) 
test2b = lequiv id (\ p -> p || p) 

test3a = lequiv    (\ p q ->      p ==> q ) (\ p q -> not p ||     q)
test3b = lequiv    (\ p q -> not (p ==> q)) (\ p q ->     p && not q)

test4a = lequiv    (\ p q -> not p ==> not q) (\ p q ->     q ==>     p)
test4b = lequiv    (\ p q ->     p ==> not q) (\ p q ->     q ==> not p)
test4c = lequiv    (\ p q -> not p ==> q)     (\ p q -> not q ==>     p)

test5a = lequiv    (\ p q ->  p <=> q) 
                   (\ p q -> (p ==> q) && (    q ==>    p))
test5b = lequiv    (\ p q ->  p <=> q) 
                   (\ p q -> (p &&  q) || (not p && not q))

test6a = lequiv    (\ p q -> p && q) (\ p q -> q && p)
test6b = lequiv    (\ p q -> p || q) (\ p q -> q || p)

test7a = lequiv    (\ p q -> not (p &&     q)) 
                   (\ p q -> not  p || not q)
test7b = lequiv    (\ p q -> not (p ||     q)) 
                   (\ p q -> not  p && not q)

test8a = lequiv    (\ p q r ->  p && (q  && r)) 
                   (\ p q r -> (p &&  q) && r)
test8b = lequiv    (\ p q r ->  p || (q  || r)) 
                   (\ p q r -> (p ||  q) || r)

test9a = lequiv    (\ p q r ->  p && (q  ||       r)) 
                   (\ p q r -> (p &&  q) || (p && r))
test9b = lequiv    (\ p q r ->  p || (q  &&       r)) 
                   (\ p q r -> (p ||  q) && (p || r))

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Theorem: 2.12/Exercise 2.13:                                             │
-- └──────────────────────────────────────────────────────────────────────────┘

-- From earlier
-- p = True  => T
-- q = False => F

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Dominace Laws                                                            │
-- │ ¬T ≡ F                                                                   │
-- │ ¬F ≡ T                                                                   │
-- │ P ==> F ≡ ¬P                                                             │
-- │ P ∨ T ≡ T                                                                │
-- │ P ∧ F ≡ F                                                                │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_12_1a = (not True) == False
test_2_12_1b = (not False) == True
test_2_12_2 = (p ==> False) == (not p)
test_2_12_3a = (p || True ) == True 
test_2_12_3b = (p && False) == False 

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Identity Laws                                                            │
-- │ P ∨ F ≡ P                                                                │
-- │ P ∧ T ≡ P                                                                │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_12_4a = (p || False) == p 
test_2_12_4b = (p && True ) == p 

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Excluded Middle Laws                                                     │
-- │ P ∨ ¬P ≡ T                                                               │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_12_5 = (p || (not p)) == True

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Contradiction                                                            │
-- │ P ∧ ¬P ≡ F                                                               │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_12_6 = (p && (not p)) == False


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.15: Propositional contradiction                               │
-- └──────────────────────────────────────────────────────────────────────────┘
-- don't undestand the question


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.16: Propositional contradiction                               │
-- └──────────────────────────────────────────────────────────────────────────┘
-- 1. The equation x^2 + 1 = 0 has a solution
-- where P = (x^2 + 1), Q = 0
-- then ¬(P ==> Q) ≡ P ∧ ¬Q
-- subs.  ¬((x^2 + 1) ==> 0) ≡ (x^2 + 1) ∧ ¬0

-- 2. A largest natural number does not exist
-- ????

-- 3. The number 13 is prime (use d|n for 'd divides n')
-- ????

-- 4. The number n is prime
-- ????

-- 5. There are infinitely mamy primes.
-- ????

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.17: x < y < z                                                 │
-- └──────────────────────────────────────────────────────────────────────────┘
-- ????

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.18: Show                                                      │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_18_1a =     (     p  <=>      q ) ==     ((not p) <=> (not q))
test_2_18_1b = not (     p  <=>      q ) == not ((not p) <=> (not q))
test_2_18_1c =     ((not p) <=> (not q)) ==     (     p  <=>      q )
 
test_2_18_2a =     ((not p) <=>      q ) ==     (     p  <=> (not q))
test_2_18_2b = not ((not p) <=>      q ) == not (     p  <=> (not q))
test_2_18_2c =     (     p  <=> (not q)) ==     ((not p) <=>      q )

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.19: Show phi = psi iff P <=> Q                                │
-- └──────────────────────────────────────────────────────────────────────────┘
test_2_19_1a = (     p  == (not q)) == (     p  <=> (not q))
test_2_19_1b = ((not p) ==      q ) == ((not p) <=>      q )

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.20: Show phi = psi iff phi <=> psi                            │
-- └──────────────────────────────────────────────────────────────────────────┘




-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.3 Making Symbolic Form Explicit                                        ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.22: between ever two rational number there is a third one.    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- (z - x) / 2 + x = y where x < z

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.31: Translate into formula                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- 1. The equation x^2 + 1 = 0 has a solution
-- 2. A largest natural number does not exist.
-- 3. The number 13 is prime (use d|n for 'd divides n').
-- 4. The number n is prime.
-- 5. There are infinitely many primes. (there are infinitely many number but 
--    not every number is a prime, thus is can not be true, but then infinity 
--    divided by some thing is stil infinity, thus yes it must be true).

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.32: Translate into formula                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- 1. Everyone loved Diana. (Use the expersion L(x,y) for x Loved y, and the 
--    name d for Diana.)
-- 2. Diana loved everyone.
-- 3. Man is mortal. (Use M(x) for 'x is a man', and ,M'(x) for 'x is mortal.)
-- 4. Some birds do not fly. (Use B(x) for 'x is a bird' and F(x) for 'x can 
--    fly'.)

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.36: Back to english                                           │
-- └──────────────────────────────────────────────────────────────────────────┘
-- 1. for some x witch is a elements of the set rational numbers, x exists 
--    where x^2 equal to 5.
-- 2. for all n witch is a element of the set natural numbers, for some m witch
--    is a element of the set natural numbers, m extist where n is smaller then
--    m.
-- 3. for all n witch is a element of the set natural numbers, for some d witch
--    is a element of the set natural numbers, d does not exist where 1 is 
--    smaller then d and 2^n + 1 or d divides by 2^n + 1.
-- 4. more complicated text!

-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.4 Lambda Abstraction                                                   ║
-- ╚══════════════════════════════════════════════════════════════════════════╝

square1 :: Integer -> Integer
square1 x = x^2 

square2 :: Integer -> Integer 
square2 = \ x -> x^2

m1 :: Integer -> Integer -> Integer 
m1 = \ x -> \ y -> x*y

m2 :: Integer -> Integer -> Integer 
m2 = \ x y -> x*y

solveQdr :: (Float,Float,Float) -> (Float,Float) 
solveQdr =  \ (a,b,c) -> if a == 0 then error "not quadratic"
                         else let d = b^2 - 4*a*c in 
                         if d < 0 then error "no real solutions"
                         else 
                           ((- b + sqrt d) / 2*a,
                            (- b - sqrt d) / 2*a)


-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.7 Logical Handling of the Quantifier                                   ║
-- ╚══════════════════════════════════════════════════════════════════════════╝ 

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.39: argue phi == psi iff phi <=> psi                          │
-- └──────────────────────────────────────────────────────────────────────────┘
-- need to use some laws...



-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.41: negations of 2.36                                         │
-- └──────────────────────────────────────────────────────────────────────────┘
-- don't get it.


-- ╔══════════════════════════════════════════════════════════════════════════╗
-- ║ 2.8 Quantifiers as Procedures                                            ║
-- ╚══════════════════════════════════════════════════════════════════════════╝ 

-- any, all :: (a -> Bool) -> [a] -> Bool
-- any p    = or  . map p
-- all p    = and . map p

every, some :: [a] -> (a -> Bool) -> Bool
every xs p = all p xs 
some  xs p = any p xs

-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.51: Unique                                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
-- don't make the array to long and it works fine
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

unique :: (a -> Bool) -> [a] -> Bool
unique c []     = False
unique c [x]    = ((length' ( filter c [x] )) == 1) <=> True 
unique c (x:xs) = ((length' ( filter c (x:xs))) == 1) <=> True 


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.52: parity                                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
parity :: [Bool] -> Bool
parity []     = False
parity [x]    = False
parity (x:xs) = (even (length' ( filter (==True) (x:xs))))
             && (0 <  (length' ( filter (==True) (x:xs))))


-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ Exercise 2.53: evenNR                                                    │
-- └──────────────────────────────────────────────────────────────────────────┘
--evenNR :: (a -> Bool) -> [a] -> Bool
-- unclear what is meant
-- VVZ: they want to check if the number of things that satisfy p is even
-- VVZ: evenNR p = even . length . filter p



