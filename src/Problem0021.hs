-----------------------------------------------------------------------------
--
--    Project Euler 0021
--     
--    Let d(n) be defined as the sum of proper divisors of n (numbers less 
--      than n which divide evenly into n).
--    If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
--      pair and each of a and b are called amicable numbers.
--
--    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 
--      22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 
--      284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--
--    Evaluate the sum of all the amicable numbers under 10000.
--
--
-----------------------------------------------------------------------------

module Problem0021 (
    run
) where

import Num.Primes

run :: IO Int
run = return $ sum (amicables 9999)

-- 'd' as defined in the problem description above
d = sum . properDivisors


                 
amicables lim = [ i | i <- [2..lim], amic i ]
             where amic i = let di   = d i 
                                ddi  = d di 
                                dddi = d ddi
                            in i == ddi && di == dddi && i /= di && di /= ddi