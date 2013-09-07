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


--exercise 2.11
--
--




