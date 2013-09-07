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
--    	          (~y => ~x) ^ (~x => ~y)  -- using laws of contraposition
--    	          (~x => ~y) ^ (~y => ~x)  -- switching operand
--    	          (~x <=> ~y)
--
--
-- 2. ~x <=> y === x <=> ~y
-- 
--    ~x <=> y === (~x => y) ^ (y => ~x)
--    		   (~y => x) ^ (x => ~y)  -- using laws of contraposition
--    		   (x => ~y) ^ (~y => x)  -- switching operand (actually this is not necessary, it easier this way)
--    	           (x <=> ~y)


-- exercise 2.19
-- x === y is true iff (if and only if) x <=> y
-- x is equivalent to y, when x and y produce the same truth value (iff x <=> y is logically valid)


-- exercise 2.20
-- 1. not
-- 2. not
-- 3. equivalent
-- 4. equivalent
-- 5. not
-- 6. not
-- 7. equivalent


-- exercise 2.21
-- 1. Q => P
-- 2. 4 rows
-- 3. TODO not sure
-- 4. TODO not sure, maybe with truth table
-- 5. 8 rows


-- exercise 2.22
-- for arbitrary x and y where x < y, there is always a rational number that satisfy x < (x+y)/2 < y


-- exercise 2.23
-- 1. All x (Ax => (Bx => Cx))
-- 	    (Ax => (Bx => Cx))
-- 	Ax		Bx => Cx
-- 		Bx			Cx
--
-- 2. There exists x (Ax ^ Bx)
-- 	     (Ax ^ Bx)
--	Ax		Bx
--
--3. There exists x (Ax) ^ There exist x (Bx)
--          (Ax) ^ There exists x (Bx)
--          	 			Bx


-- exercise 2.26
-- 1. There exists x element Q; There exists y element Q (x < y)
-- 2. For all x element R; There exists y element R (x < y)
-- 3. For all x element Z; There exists m,n element N (x = m - n)


-- exercise 2.27
-- 1. For all x (x element Q => There exist m,n (m element Z ^ n element Z ^ n /= 0 ^ x = m/n))
-- 2. For all x(Fx => For all y(Dy => (Oxy => Bxy)))


-- exercise 2.31
-- 1. There exists x (x^2 + 1 == 0)
-- 2. There exists x element N; For all y element N (x >= y)
-- 3. TODO not sure
-- 4. TODO not sure
-- 5. TODO not sure


-- exercise 2.32 
-- assumption that x is element of Human
-- 1. For all x (L(x,d))
-- 2. For all x (L(d,x))
-- 3. For all x (M(x) => M'(x))
-- 4. There exists x (B(x) ^ ~F(x))


-- exercise 2.33
-- 1. There exists x ((D(x) ^ B(x)) => ~Bite(x))
-- 2. For all x (G(x) => ~Gold(x))
-- 3. For all x,y ((Fxy ^ Fyd) => Fxd)
-- 4. For all x > 0; y element N; z element N (z >= y => 1/z < x)


-- exercise 2.34
-- 1. For all x (x /= charles => L(x,d))
-- 2. TODO check this: For all x (M(x) => A(x) > 2)
-- 3. TODO check this: For all x (M(x) => Married(x) < 2)


-- exercise 2.35
-- 1. There exists x (King(x) ^ For all y (King(y) => y = x) ^ ~Raging(x))
-- 2. There exists x (King(x) ^ For all y (King(y) => y = x) ^ For all z (S(z,x) => Love(z,x)))


-- exercise 2.36
-- 1a. There exists x element R such that x^2 is 5
-- 1b. The equation of x^2 = 5 has a real solution
--
-- 2a. For all n element N; There exists m element N such that n < m
-- 2b. There is no largest natural number
--
-- 3a. For all n element N; There not exists d element N such that (1<d<(2^n+1)) and d|(2^n + 1)
-- 3b. For all natural numbers n, it holds that 2^n + 1 is prime
--
-- 4a. For all n element N; There exists m element N such that (n < m) and For all p element N such that (p <= n || m <= p)
-- 4b. every natural number has an immediate successor
--
-- 5a. For all e element positive R; There exists n,m element N such that m >= n implies that (|a-am| <= e)
-- 5b. TODO not sure


-- exercise 2.37
-- 1. a. f	b. f	c. f	d. f	e. f	f. f
-- 2. a. t	b. f	c. t	d. f	e. f	f. f
-- 3. a. f	b. f	c. f	d. f	e. f	f. t
-- 4. a. t	b. f	c. f	d. f	e. f	f. t
-- 5. a. t	b. f	c. f	d. f	e. f	f. f


-- exercise 2.38 
-- 1. a. f	b. f	c. f	d. f	e. f	f. f
-- 2. a. t	b. f	c. f	d. f	e. f	f. f
-- 3. a. f	b. f	c. f	d. f	e. f	f. t
-- 4. a. t	b. f	c. f	d. f	e. f	f. t
-- 5. a. t	b. f	c. f	d. f	e. f	f. f


-- exercise 2.39
-- x => y && y => x are true in any context


-- exercise 2.41
-- A is for All; E is for there exists
-- 1. Ax (x^2 /= 5) === ~Ex (x^2 = 5)
-- 2. En Am (n >= m) === ~An Em (n < m)
-- 3. En Ad (1>=d>=(2^n+1) V d/|(2^n+1)) === ~An ~Ed (1<d<(2^n+1) ^ d|(2^n+1))
-- 4 and 5 applies the same principles.

-- exercise 2.46
-- No. it should be: All x element A ~(f(x))
-- Statement 1 -> no cat is kind
-- Statement 2 -> there exist other than cat that is kind


-- exercise 2.47
-- No.
-- Statement 1 -> there is other than cat that is not kind
-- Statement 2 -> there exists cat that is not kind


-- exercise 2.50
-- Ex > 0 An Em >= n(|a-am| >= x)


-- exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1


-- exercise 2.52
getEvenElementFromList :: [a] -> [a] 
getEvenElementFromList [] = []
getEvenElementFromList [x] = []
getEvenElementFromList (x:y:xs) = y : getEvenElementFromList (xs)   

-- gives True only when even position of the list compute to True
-- e.g. parity [False, True, False, True] will be True
parity :: [Bool] -> Bool
parity xs = and (getEvenElementFromList xs) 


-- exercise 2.53
-- gives True when a list lenght is even number that complies with p
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = even (length(filter p xs))

