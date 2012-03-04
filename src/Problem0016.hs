-----------------------------------------------------------------------------
--
--    Project Euler 0016
--   
--    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
--    What is the sum of the digits of the number 2^1000?
--
--
-----------------------------------------------------------------------------

module Problem0016 (
    run
) where

import Data.List (foldl')
import Data.Char (digitToInt)

run :: IO Int
run = return $ naiveApproach 1000

naiveApproach :: Integer -> Int
naiveApproach n = foldl' (+) 0 $ map digitToInt $ show (2^n ::Integer)