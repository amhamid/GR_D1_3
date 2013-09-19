-- Compile a .hs file
--
-- C:\>cd C:\Users\stevan\Desktop\UvA\Software Testing\Week 1
-- C:\..\>ghci
-- Prelude> :! ghc -c lib/GS.hs
-- Prelude> :! ghc -c lib/TAMO.hs
-- Prelude> :q

-- Start GHCi with a lib dir included and load your Program/Script
--
-- C:\>cd C:\Users\stevan\Desktop\UvA\Software Testing\Week 1
-- C:\..\>ghci -ilib/
-- Prelude> :l chapter-1/stevan/Sol1

-- Show currently loaded modules
--
-- *Sol1> :show modules
-- Sol1             ( chapter-1\stevan\Sol1.hs, interpreted )
-- GS               ( lib\GS.hs, lib\GS.o )

module Sol2 where

import GS
import TAMO


-- Exercise 2.2
-- P Q	P XOR Q
-- t t	   f
-- t f     t
-- f t     t
-- f f     f

-- Exercise 2.4
-- P XOR Q	!(P<=>Q)
--    f         f
--    t         t
--    t         t
--    f         f
--
-- I have no idea how to conclude that the function infixr 2 <+> is the
-- correct implementation of XOR, because I can't read what it says/don't 
-- understand the syntax/grammar.

-- Exercise 2.9
-- P XOR Q == R XOR Q == P
-- t     t    f     t    t
-- t     f    t     f    t
-- f     t    t     t    f
-- f     f    f     f    f

-- Exercise 2.11
-- I don't know how to do it.

-- Exercise 2.13
-- I don't know how to do it.

-- Exercise 2.15
-- I think I lost my way in Chapter 2.2.
-- VVZ: try to re-read it and ask questions about what you don't understand
-- VVZ: another way would be to google up some real Haskell code and read that in order to get the feel of the language, if that is the problem
