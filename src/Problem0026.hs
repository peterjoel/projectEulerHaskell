-----------------------------------------------------------------------------
--
--  Project Euler 26
--
--    A unit fraction contains 1 in the numerator. The decimal representation 
--    of the unit fractions with denominators 2 to 10 are given:
--
--        1/2	= 	0.5
--        1/3	= 	0.(3)
--        1/4	= 	0.25
--        1/5	= 	0.2
--        1/6	= 	0.1(6)
--        1/7	= 	0.(142857)
--        1/8	= 	0.125
--        1/9	= 	0.(1)
--        1/10	= 	0.1
--
--    Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It 
--    can be seen that 1/7 has a 6-digit recurring cycle.
--
--    Find the value of d < 1000 for which 1/d contains the longest recurring 
--    cycle in its decimal fraction part.
--
--
-----------------------------------------------------------------------------

module Problem0026 (
    run
)
where

import Data.List
import Data.Function


run :: IO Int
run = return $ calc 999


calc :: Int -> Int
calc n = maxIndex $ map (cycleLen . fracDigitsWithRem . reduce) [1..n]
    where -- dividing by 2 or 5 doesn't change the cycle length, but gets 
          -- rid of the leading non-cycling digits
          reduce            = reduceBy 2 . reduceBy 5
          reduceBy p        = until ((/=0).(`mod` p)) (`div` p)
          
          maxIndex          = fst . maximumBy (compare `on` snd) . zip [1..]
          
          -- assumes that all leading non-cycles are stripped
          cycleLen (x:xs)   = fst $ head $ dropWhile ((/=x).snd) (zip [1..] xs)



-- | List of decimal digits, calculated by long division, paired with each 
--   digit's division remainder
fracDigitsWithRem :: Int -> [(Int, Int)]
fracDigitsWithRem n = tail $ digits 1
  where digits p = (k, r) : digits (r*10)
          where (k, r) = divMod p n

