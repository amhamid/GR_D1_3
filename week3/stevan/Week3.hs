module Week3 where

import Week3FromLecture
import TechniquesWeek3FromLecture
import System.Random
import Data.List
import System.IO

genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d n = do
				f <- getRandomInt d
				fs <- genIntList' d (n-1)
				return (f:fs)

genIntList :: IO [Int]
genIntList = genIntList' 100 10

genIntList2 :: IO [Int]
genIntList2 = do n <- getRandomInt 10
                 d <- getRandomInt 10
                 getRandomItems d n

getRandomItems :: Int -> Int -> IO [Int]
getRandomItems _ 0 = return []
getRandomItems d n = do 
                     f  <- getRandomInt d
                     fs <- getRandomItems d (n-1) 
                     return (f:fs)


					 
					 
					 
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []    []     = False
isPermutation [x]   []     = False
isPermutation []    [y]    = False
isPermutation [xs]  [ys]    = any ([x]==) (permutations [y])
isPermutation (x:xs) (y:ys) = any ((x:xs)==) (permutations (y:ys))



-- testIsPermutation :: Int -> Int -> Bool
-- testIsPermutation x y = x != y
-- testIsPermutation isPermutation getRandomItems genIntList2

--saveArr :: IO ()
saveArr = do
    outh <- openFile "test.txt" WriteMode
	--x = putStrLn "blaat"
    hPrint outh "[1,2,3]"
    hClose outh


setVarWithString :: String -> IO String
setVarWithString s = do return s

testCNF :: IO ()
testCNF = do        
  writeFile "Result.txt" "test\n"
  x <- (setVarWithString ("ssss" ++ show 1 ++ " blabla"))
--  x <- (putStrLn "ssss")
  appendFile "Result.txt" x

  

  
  
  
  
  
  
  
  
  
  











