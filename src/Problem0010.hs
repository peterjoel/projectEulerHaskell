-----------------------------------------------------------------------------
--
--    Project Euler 0010
--   
--    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
--    Find the sum of all the primes below two million.
--
-----------------------------------------------------------------------------

module Problem0010 (
    run
) where



import Num.Primes

run :: IO Integer
run = return $ calc 2000000


calc :: Int -> Integer
calc m = ( sum . (map fromIntegral) . takeWhile (<m)) allPrimes



