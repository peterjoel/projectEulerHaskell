-----------------------------------------------------------------------------
--
--    Project Euler 0013
--   
--
--   Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
--   37107287533902102798797998220837590246510135740250
--   ... (data/problem0013.txt)
--
-----------------------------------------------------------------------------

module Problem0013 (
    run
) where

import Paths_projectEuler
import Data.Char
import Data.Functor

run :: IO Int
run = (read . take 10 . show . foldl (+) 0) <$> input


input :: IO [Integer]
input = do file <- readFile =<< getDataFileName "problem0013.txt"
           let toIntLines = read . filter (`elem` ['0'..'9'])
           return $ (map toIntLines . lines) file
           
           
