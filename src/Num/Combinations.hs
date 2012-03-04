-----------------------------------------------------------------------------
--
-- Module      :  Num.Combinations
-- Copyright   :  
-- License     :  AllRightsReserved
--
-- Maintainer  :  
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Num.Combinations (
     factorial
    ,combinations
) where


combinations n k 
    | k == n  = 1
    | k == 0  = 1
    | k > n   = error "k should be no larger than n"
    | otherwise = (factorial n) `div` (factorial k * factorial (n-k))


factorial n = fact' n 1
          where fact' 0 a = a
                fact' n a = fact' (n-1) $! (n*a)