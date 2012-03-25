

module Num.Digits (
     digits 
    ,digitsD
    ,digitsB
    ,fromDigits
    ,fromDigitsB
    ,fromDigitsD
) where

import Data.Char (digitToInt)
import Data.List (insert, foldl1')


{-# INLINABLE digitsD #-}
digitsD :: Integral a => a -> [a]
digitsD = digits 10

            
{-# INLINABLE fromDigitsD #-}
fromDigitsD :: Integral a => [a] -> a                                   
fromDigitsD = fromDigits 10



{-# INLINABLE digitsB #-}
digitsB :: Integral a => a -> [a]
digitsB = digits 2

{-# INLINABLE fromDigitsB #-}
fromDigitsB :: Integral a => [a] -> a                                   
fromDigitsB = fromDigits 2


{-# INLINABLE digits #-}
digits :: Integral a => a -> a -> [a]
digits b 0 = [0]
digits b n = reverse $ digits' n
    where digits' 0 = []
          digits' n = r : digits' q
            where (q,r) = quotRem n b
            
{-# INLINABLE fromDigits #-}
fromDigits :: Integral a => a -> [a] -> a                                   
fromDigits b = foldl1' (\i j -> b * i + j)



