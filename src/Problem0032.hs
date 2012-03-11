-----------------------------------------------------------------------------
--
--  Project Euler 32
--
--
--    We shall say that an n-digit number is pandigital if it makes use of 
--    all the digits 1 to n exactly once; for example, the 5-digit number, 
--    15234, is 1 through 5 pandigital.
--
--    The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
--    containing multiplicand, multiplier, and product is 1 through 9 
--    pandigital.
--
--    Find the sum of all products whose multiplicand/multiplier/product 
--    identity can be written as a 1 through 9 pandigital.
--
--    HINT: Some products can be obtained in more than one way so be sure 
--    to only include it once in your sum.
--
--
-----------------------------------------------------------------------------
{-
    Pretty literal, inefficient translation of the problem. 
    
    Using lists of digits and converting to int explicitly is more than 10x
    faster than using String representations, though more verbose.
-}
module Problem0032 (
    run
) where

import Data.List (permutations,nub)

run :: IO Int
run = return $ calc [1..9]


calc = sum . nub . map third . filter isProd . concat . map parts . permutations
    where third (_,_,b)          = toInt b
          isProd (a,b,p)         = toInt a * toInt b == toInt p
          -- All possibilities have digits: A x AAAA or AA x AAA
          parts (a:b:c:d:e:rest) = [([a], [b,c,d,e], rest)
                                   ,([a,b], [c,d,e], rest)]
          toInt as               = sum $ map pwr $ zip (reverse as) [0..]
          pwr (x,e)              = x * 10^e

