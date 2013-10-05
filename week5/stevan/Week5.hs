module Week5

where

import Data.List
import Week5FromLecture
import RandomSudoku
import Control.Exception (assert)


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

{-
4. The course notes of this week contain a sudoku solver. A sudoku generator written
in Haskell is available on the course web page, as RandomSudoku.hs. Use your
program from the previous exercise and this program to create a program that
generates NRC-Handelsblad sudoku problems.

Deliverables: NRC-Handelsblad sudoku generator, indication of time spent.
-}

