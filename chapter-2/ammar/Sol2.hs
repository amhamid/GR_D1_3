module Sol2 where

import GS	
import TAMO


-- exercise 2.2
-- P	Q	XOR
-- t	t	 f
-- t	f	 t
-- f	t	 t
-- f	f	 f


-- exercise 2.4
-- P 	Q	P XOR Q		P<=>Q	~(P<=>Q)
-- t	t	   f		  t	    f
-- t	f	   t		  f	    t
-- f	t	   t		  f	    t
-- f	f	   f		  t	    f
--
-- The implementation of XOR: x <+> y = x /= y is correct.


-- exercise 2.9
-- P 	Q	R = (P XOR Q)	R XOR Q 
-- t	t	     f		   t
-- t	f	     t		   t
-- f	t	     t		   f
-- f	f	     f		   f


-- exercise 2.11
-- P	Q	~(P ^ Q)	~P V ~Q
-- t	t	    f		   f
-- t	f	    t		   t
-- f	t	    t		   t
-- f	f	    t	    	   t


-- exercise 2.13
propositionTest1a = (not True) <=> False
propositionTest1b = (not False) <=> True
propositionTest2 = logEquiv1 (\ p -> p ==> False) (\p -> not p)
propositionTest3a = logEquiv1 (\ p -> p || True) (\ p -> True)
propositionTest3b = logEquiv1 (\ p -> p && False) (\ p  -> False) 
propositionTest4a = logEquiv1 id (\ p -> p || False) 
propositionTest4b = logEquiv1 id (\ p -> p && True) 
propositionTest5 = logEquiv1 (\ p -> p || not p) (\ p -> True)
propositionTest6 = logEquiv1 (\ p -> p && not p) (\ p -> False)


-- exercise 2.15
contra1 ::(Bool -> Bool) -> Bool
contra1 f = not (f True) && not (f False)

contra2 :: (Bool -> Bool -> Bool) -> Bool
contra2 f = and [not (f p q) | p <- [True, False],
			       q <- [True, False]]

contra3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contra3 f = and [not (f p q r) | p <- [True, False],
			         q <- [True, False],
			         r <- [True, False]]


-- exercise 2.16
-- TODO not sure how to do that 


-- exercise 2.17
-- x < y < z is equivalent with x < y ^ y < z ; so the negation is x >= y V y >= z


-- exercise 2.18 
-- 1. x <=> y === ~x <=> ~y 
--
--    x <=> y === (x => y) ^ (y => x) 
--    	          (~x => ~y) ^ (~y => ~x)   ----> using Subtitution Principle: x with ~x and y with ~y
--    	          (~x <=> ~y)
--
--
-- 2. ~x <=> y === x <=> ~y
-- 
--    ~x <=> y === (~x => y) ^ (y => ~x)
--    	           (x => ~y) ^ (~y => x)   ----> using Subtitution Principle: ~x with x and y with ~y 
--    	           (x <=> ~y)


-- exercise 2.19
-- x === y is true iff (if and only if) x <=> y
-- x is equivalent to y, when x and y produce the same truth value (iff x <=> y is logically valid)


-- exercise 2.20
--















