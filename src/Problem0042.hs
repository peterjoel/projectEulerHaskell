-----------------------------------------------------------------------------
--
--   Project Euler 42
--
--
--    The nth term of the sequence of triangle numbers is given by, 
--        tn = ½n(n+1)
--
--    so the first ten triangle numbers are:
--
--    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
--    By converting each letter in a word to a number corresponding to its 
--    alphabetical position and adding these values we form a word value. 
--    For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the 
--    word value is a triangle number then we shall call the word a triangle 
--    word.
--
--    Using words.txt (right click and 'Save Link/Target As...'), a 16K text 
--    file containing nearly two-thousand common English words, how many are 
--    triangle words?
--
--
-----------------------------------------------------------------------------

module Problem0042 (
    run
) where

import Control.Applicative
import Data.Char
import Data.List (groupBy)
import Data.Function (on)
import Data.Array

import Paths_projectEuler (getDataFileName)


run :: IO Int
run = return =<< calc <$> input

calc :: [String] -> Int
calc ws = length $ filter isTriangle (map wordNum ws)
      where isTriangle n = trisArray ! n == True
                where triangles = [(1+n) * n `div` 2 | n <- [1..]] 
                      trisArray = listArray (0,arrayLen) (replicate arrayLen False) 
                                     // zip (takeWhile (<=arrayLen) triangles) (repeat True)
                      arrayLen = 1 + (maximum $ map wordNum ws)
  
wordNum :: String -> Int  
wordNum w = sum $ map ((subtract 64) . fromEnum) w                                 

input :: IO [String]
input = do file <- readFile =<< getDataFileName "problem0042.txt"
           return $ filter (isAlpha . head) $ groupBy ((==) `on` isAlpha) file



