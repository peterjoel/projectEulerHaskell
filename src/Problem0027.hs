-----------------------------------------------------------------------------
--
--  Project Euler 27
--
--    Euler published the remarkable quadratic formula:
--
--    n² + n + 41
--
--    It turns out that the formula will produce 40 primes for the consecutive 
--    values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 
--    is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly 
--    divisible by 41.
--
--    Using computers, the incredible formula  n² − 79n + 1601 was discovered, 
--    which produces 80 primes for the consecutive values n = 0 to 79. The 
--    product of the coefficients, −79 and 1601, is −126479.
--
--    Considering quadratics of the form:
--
--        n² + an + b, where |a| < 1000 and |b| < 1000
--
--        where |n| is the modulus/absolute value of n
--        e.g. |11| = 11 and |−4| = 4
--
--    Find the product of the coefficients, a and b, for the quadratic expression 
--    that produces the maximum number of primes for consecutive values of n, 
--    starting with n = 0.
--
--
-----------------------------------------------------------------------------
{-
    This is a brute force approach, made even slower by the naive
    implementation of isPrime
    
    Runs in around 6 seconds :(
-}
module Problem0027 (
    run
) where

import Data.Function
import Data.List
import Num.Primes


range = [-999..999]

run :: IO Int
run = return $ a * b
    where (_,(a,b)) = maximumBy (compare `on` fst) [ (numPrimesFor a b, (a,b)) 
                                                        | a <- range
                                                        , b <- range ]


numPrimesFor a b = snd $ head $ dropWhile (isPrime . fst) $ zip values [0..]
    where values = [ n^2 + a*n + b | n <- [0..]]
    
    
    
