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


