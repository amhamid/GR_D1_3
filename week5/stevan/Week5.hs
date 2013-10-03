module Week5

where

import Data.List
import Week5_2
import RandomSudoku
import Control.Exception (assert)


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

