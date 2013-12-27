

module Num.Digits (
     digits 
    ,digitsD
    ,digitsB
    ,fromDigits
    ,fromDigitsB
    ,fromDigitsD
    ,toDigits
    ,toDigitsB
    ,toDigitsD
) where

import Data.Char (digitToInt)
import Data.List (insert, foldl')


{-# INLINABLE digitsD #-}
digitsD :: Integral a => a -> [a]
digitsD = digits 10

            
{-# INLINABLE fromDigitsD #-}
fromDigitsD :: Integral a => [a] -> a                                   
fromDigitsD = fromDigits 10

{-# INLINABLE toDigitsD #-}
toDigitsD :: Integral a => a -> [a]                             
toDigitsD = toDigits 10


{-# INLINABLE toDigitsB #-}
toDigitsB :: Integral a => a -> [a]                             
toDigitsB = toDigits 2


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
fromDigits b = foldl' (\i j -> b * i + j) 0

{-# INLINABLE toDigits #-}
toDigits :: Integral a => a -> a -> [a]
toDigits base n = toD [] n
        where toD ds 0 = ds 
              toD ds n = let (d, r) = divMod n base 
                         in toD (r:ds) d





