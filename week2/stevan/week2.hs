module Week2

where

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
                 
--------------------------------------------------------------------------------
-- Test Report
--
-- 
--
--
--
--
--
--
--
--
--
--
--
--
--  main = print (  triangle (-1) 4 4,  -- NoTriangle
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
