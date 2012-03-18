

module Num.Digits (
     digits 
    ,fromDigits
) where

import Data.Char (digitToInt)
import Data.List (foldl1')


{-# INLINABLE digits #-}
digits :: Integral a => a -> [Int]
-- this can probably be made faster by not using Show
digits = map digitToInt . show 

{-# INLINABLE fromDigits #-}
fromDigits :: Integral a => [a] -> a                                   
fromDigits = foldl1' (\a b -> 10 * a + b)