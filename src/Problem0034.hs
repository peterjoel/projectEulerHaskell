-----------------------------------------------------------------------------
--
--  Project Euler 34
--
--
--    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
--    Find the sum of all numbers which are equal to the sum of the factorial 
--    of their digits.
--
--    Note: as 1! = 1 and 2! = 2 are not sums they are not included.
--
--
-----------------------------------------------------------------------------

module Problem0034 (
    run
) where

import Data.Char
import Num.Combinations

run :: IO Int
run = return calc

calc = sum $ filter isCurious [3..limit]
    where isCurious n = n == (sum $ map (factorial . digitToInt) $ show n)
          limit = f * (length . show) f
                where f = factorial 9

