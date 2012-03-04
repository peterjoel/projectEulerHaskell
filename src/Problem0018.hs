-----------------------------------------------------------------------------
--
--    Project Euler 0018
--   
--    By starting at the top of the triangle below and moving to adjacent numbers on 
--      the row below, the maximum total from top to bottom is 23.
--
--    3
--    7 4
--    2 4 6
--    8 5 9 3
--
--    That is, 3 + 7 + 4 + 9 = 23.
--
--    Find the maximum total from top to bottom of the triangle below:
--
--                   75
--                  95 64
--                 17 47...
--
--    NOTE: As there are only 16384 routes, it is possible to solve this problem 
--      by trying every route. However, Problem 67, is the same challenge with a 
--      triangle containing one-hundred rows; it cannot be solved by brute force, 
--      and requires a clever method! ;o)
--
--
-----------------------------------------------------------------------------
--
--  The idea is to start at the bottom row and keep track of the total for 
--  each subtree, until we get to the top
--
-----------------------------------------------------------------------------

module Problem0018 (
     run
    ,runWithFile
) where


import Data.Functor
import Paths_projectEuler (getDataFileName)


data Tree a = Branch a (Tree a) (Tree a) | Leaf a

run :: IO Int
run = runWithFile "problem0018.txt"

runWithFile :: String -> IO Int
runWithFile f = sumPaths <$> input f

input :: String -> IO [[Int]]
input f = do file <- readFile =<< getDataFileName f
             return $ reverse . map (map read . words ) $ lines file


sumPaths :: [[Int]] -> Int
sumPaths (r:rows) = head $ sumPaths' r rows
      where sumPaths' sums []       = sums
            sumPaths' sums (r':rows') = sumPaths' (zipWith (+) maxes r') rows'
                where maxes = zipWith max sums (tail sums)



