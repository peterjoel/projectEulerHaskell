-----------------------------------------------------------------------------
--
--  Project Euler 35
--
--
--  The number, 197, is called a circular prime because all rotations of the 
--  digits: 197, 971, and 719, are themselves prime.
--
--  There are thirteen such primes below 100:
--     2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
--  How many circular primes are there below one million?
--
--
-----------------------------------------------------------------------------

module Problem0035 (
    run
) where


import Num.Digits
import Num.Primes
import Data.List (sort, group)
import GHC.Exts (sortWith)

run :: IO Int
run = return $ calc


calc = length 
        $ concat $ filter hasAllRotations 
        $ group $ sortWith fromDigitsD
        $ map (sort . digitsD) 
        $ takeWhile (<1000000) allPrimes
        
        where -- only actually check the number of rotations, since they've 
              -- been sorted and are all the same
              hasAllRotations group 
                    | length (head group) == 1               = True
                    | any (`notElem` [1,3,7,9]) $ head group = False
                    | all (==1) $ head group                 = True
                    | length group == length (head group)    = True
                    | otherwise                              = False