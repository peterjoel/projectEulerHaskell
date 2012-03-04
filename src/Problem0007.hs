-----------------------------------------------------------------------------
--
--    Project Euler 0007
--   
--    By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--
--    What is the 10 001st prime number?
--
--    A: 104743
--
-----------------------------------------------------------------------------

module Problem0007 (
    run
) where


import Num.Primes

run :: IO Int
run = return $ prime 10001


