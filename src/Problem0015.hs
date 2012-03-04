-----------------------------------------------------------------------------
--
--    Project Euler 0015
--   
-- 
--    Starting in the top left corner of a 2×2 grid, there are 6 routes 
--    (without backtracking) to the bottom right corner.
--
--    How many routes are there through a 20×20 grid?
--
--
-----------------------------------------------------------------------------
--  I think this means you can only go down or right
--  
--  You always have to go down 20 times and right 20 times, in any order, so
--  it's the number of ways or ordering these:
--   0000000000000000000011111111111111111111
--
-----------------------------------------------------------------------------

module Problem0015 (
    run
) where


import Num.Combinations

run :: IO Integer
run = return $ combinations 40 20
