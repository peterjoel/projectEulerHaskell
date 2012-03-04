-----------------------------------------------------------------------------
--
--    Project Euler 0005
--   
--    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--
--    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--
-----------------------------------------------------------------------------

module Problem0005 (
    run
) where


run :: IO Int
run = return $ foldr lcm 2 [3..20]
