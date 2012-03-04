-----------------------------------------------------------------------------
--
--    Project Euler 0008
--   
--    Find the greatest product of five consecutive digits in the 1000-digit number.
--
--    73167176531330624919225119674426574742355349194934
--    96983520312774506326239578318016984801869478851843
--    85861560789112949495459501737958331952853208805511
--    125406987471585238... (data/problem0008.txt)
--
-----------------------------------------------------------------------------

module Problem0008 (
    run
) where


import Data.Char
import Paths_projectEuler (getDataFileName)

run :: IO Int
run = fmap (\d -> calc d 0 5) input 

input :: IO String
input = do file <- readFile =<< getDataFileName "problem0008.txt"
           return $ filter (`elem` ['0'..'9']) file

calc d m s
        | length d < s = m
        | otherwise = calc (tail d) (max m $ f $ take s d) s
        where f = foldl (\a -> (a*) . digitToInt) 1


    