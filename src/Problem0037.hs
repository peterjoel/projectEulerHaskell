-----------------------------------------------------------------------------
--
--  Project Euler 37
--
--
--    The number 3797 has an interesting property. Being prime itself, it 
--	  is possible to continuously remove digits from left to right, and remain 
--    prime at each stage: 3797, 797, 97, and 7. Similarly we can work from 
--    right to left: 3797, 379, 37, and 3.
-- 
-- 	  Find the sum of the only eleven primes that are both truncatable from 
-- 	  left to right and right to left.
--
--    NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
--
-----------------------------------------------------------------------------
{-
    Solutions must:
    	1. Start with prime (2,3,5,7)
        2. Last digit is 3 or 7
	    3. all other digits are 1,3,7,9

    Do a breadth-first search, to find all right-reducable primes, then pick the
    solutions that are also left-reducable
-}
module Problem0037 (
    run
) where

import Num.Digits
import Num.Primes
import Control.Applicative

run :: IO Int
run = return . sum . take 11 . filter isLeftTruncatable $ rightTruncatables 

rightTruncatables :: [Int]
rightTruncatables = concat . tail . iterate next $ [2,3,5,7] 
        where next ps = filter isPrime 
                            $ (\a b -> 10*a+b) <$> ps <*> [1,3,7,9]

isLeftTruncatable :: Int -> Bool
isLeftTruncatable = isLT . toDigits 10
        where isLT []     = True
              isLT [p]    = True
              isLT (_:ps) = (isPrime . fromDigits 10 $ ps) && isLT ps



